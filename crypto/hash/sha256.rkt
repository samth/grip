#lang typed/racket/base

(provide sha256 sha256-bytes)

(require 
 racket/pretty
 racket/fixnum
 (only-in "../../r6rslib/bytevectors.rkt"
	  bytevector?
	  string->utf8
	  make-bytevector
	  bytevector-copy
	  bytevector-copy!
	  bytevector-fill!
	  bytevector-u32-set!
	  bytevector-u32-ref
	  bytevector-length
	  bytevector-u8-ref
	  bytevector-u8-set!)
 (only-in "../../r6rslib/arithmetic/bitwise.rkt"
          bitwise-arithmetic-shift-right
          bitwise-arithmetic-shift-left)
 (only-in "../private/util.rkt"
          fxdiv-and-mod fx1+ fx1- fxzero? str->bv))

;; (require (filtered-in
;;           (λ (name) (regexp-replace #rx"unsafe-" name ""))
;;           racket/unsafe/ops))

 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; SHA-256 (SHA2) from FIPS PUB 180-3
;; Works with a in-memory bytevector of data.
;; i.e., no support for a byte stream which could be easily added.
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Mask for module 32 arithmetic
;; (: w32 Integer)
(: w32 Integer)
(define w32 #xffffffff)

;; truncate an integer to module 32 value.
(: m32 (Integer -> Integer))
(define (m32 x)
  (bitwise-and x w32))

;; Add to int values module 32 arithmetic
(: m32+ (Integer Integer -> Integer))
(define (m32+ x y)
  (m32 (+ x y)))

;; set the ith u32 location to the  u32 value 
(: word-set! (Bytes Integer Integer -> Void))
(define (word-set! bv32 idx val)
  (bytevector-u32-set! bv32 (fx* 4 idx) val 'big))

;; the value of the ith u32 location
(: word-ref (Bytes Integer -> Integer))
(define (word-ref bv32 idx)
  (bytevector-u32-ref bv32 (fx* 4 idx) 'big))

;; SHA-256 right rotate 
(: rotate (Integer Integer -> Integer))
(define (rotate v n)
  (m32 (bitwise-ior (bitwise-arithmetic-shift-right v n)
		    (bitwise-arithmetic-shift-left v (fx- 32 n)))))

;; SHA-256 initial hash values.
(define H0 #x6a09e667)
(define H1 #xbb67ae85)
(define H2 #x3c6ef372)
(define H3 #xa54ff53a)
(define H4 #x510e527f)
(define H5 #x9b05688c)
(define H6 #x1f83d9ab)
(define H7 #x5be0cd19)

;; SHA-256 round constants.
(: k Bytes)
(define k
  (do ((constants (make-bytevector (fx* 64 4) 0))
       (idx 0 (fx1+ idx))
       (round-values 
	(list #x428a2f98 #x71374491 #xb5c0fbcf #xe9b5dba5 #x3956c25b #x59f111f1 #x923f82a4 #xab1c5ed5
	      #xd807aa98 #x12835b01 #x243185be #x550c7dc3 #x72be5d74 #x80deb1fe #x9bdc06a7 #xc19bf174 
	      #xe49b69c1 #xefbe4786 #x0fc19dc6 #x240ca1cc #x2de92c6f #x4a7484aa #x5cb0a9dc #x76f988da 
	      #x983e5152 #xa831c66d #xb00327c8 #xbf597fc7 #xc6e00bf3 #xd5a79147 #x06ca6351 #x14292967 
	      #x27b70a85 #x2e1b2138 #x4d2c6dfc #x53380d13 #x650a7354 #x766a0abb #x81c2c92e #x92722c85 
	      #xa2bfe8a1 #xa81a664b #xc24b8b70 #xc76c51a3 #xd192e819 #xd6990624 #xf40e3585 #x106aa070 
	      #x19a4c116 #x1e376c08 #x2748774c #x34b0bcb5 #x391c0cb3 #x4ed8aa4a #x5b9cca4f #x682e6ff3 
	      #x748f82ee #x78a5636f #x84c87814 #x8cc70208 #x90befffa #xa4506ceb #xbef9a3f7 #xc67178f2) 
	(cdr round-values)))
      ((fx= idx 64) constants)
    (word-set! constants idx (car round-values))))

;; Expand a 64 byte chunk
(: expand-chunk! (Bytes -> Void))
(define (expand-chunk! w64)
  (do: : Void ((i : Fixnum 16 (fx1+ i)))
       ((fx= i 64))
       (let ((s0 (let ((w (word-ref w64 (fx- i 15))))
		   (bitwise-xor (rotate w 7)
				(rotate w 18)
				(bitwise-arithmetic-shift-right w 3))))
	     (s1 (let ((w (word-ref w64 (fx- i 2))))
		   (bitwise-xor (rotate w 17)
				(rotate w 19)
				(bitwise-arithmetic-shift-right w 10)))))
	 (word-set! w64 i (m32+ (word-ref w64 (fx- i 16))
				(m32+ s0 (m32+ (word-ref w64 (fx- i 7)) s1)))))))

;; set the data length in # of bits in the chunk
(: chunk-data-length! (Bytes Integer -> Void))
(define (chunk-data-length! w64 len)
  (let ((bits (* len 8)))
    (let ((low (m32 bits))
	  (high (m32 (bitwise-arithmetic-shift-right bits 32))))
      (word-set! w64 14 high)
      (word-set! w64 15 low))))

(: sha256-bytes (Bytes -> Bytes))
(define (sha256-bytes bv)
  (let ((len (bytevector-length bv))  ;; size of data to hash
	(w64 (make-bytevector 256 0)) ;; working buffer 64 32-bit words (256 bytes)
	(h0 H0)(h1 H1)(h2 H2)(h3 H3)(h4 H4)(h5 H5)(h6 H6)(h7 H7)) ;; init the hash
    
    ;; process all the 64 bytes (512 bit) chunks available
    (let-values (((chunks remainder)(fxdiv-and-mod len 64))) ;; # of 64 byte chunks and remainder
      (let ((hash-and-update 	       ;; Hash the chunk and update running hash values
	     (lambda ()
	       (let ((a h0)(b h1)(c h2)(d h3)(e h4)(f h5)(g h6)(h h7))
		 (do ((i 0 (fx1+ i)))
		     ((fx= i 64))
		   (let ((s0  (bitwise-xor (rotate a 2) (rotate a 13) (rotate a 22)))
			 (maj (bitwise-xor (bitwise-and a b) (bitwise-and a c) (bitwise-and b c))))
		     (let ((t2 (m32+ s0 maj))
			   (s1 (bitwise-xor (rotate e 6) (rotate e 11) (rotate e 25)))
			   (ch (bitwise-xor (bitwise-and e f) (bitwise-and (bitwise-not e) g))))
		       (let ((t1 (m32+ h (m32+ s1 (m32+ ch (m32+ (word-ref k i) (word-ref w64 i)))))))
			 (set! h g)(set! g f)(set! f e)(set! e (m32+ d t1))(set! d c)(set! c b)(set! b a)
			 (set! a (m32+ t1 t2))))))
		 ;; update hash with chunk's hashed values
		 (set! h0 (m32+ h0 a))(set! h1 (m32+ h1 b))(set! h2 (m32+ h2 c))
		 (set! h3 (m32+ h3 d))(set! h4 (m32+ h4 e))(set! h5 (m32+ h5 f))
		 (set! h6 (m32+ h6 g))(set! h7 (m32+ h7 h))))))
	
	;; hash all full 64 byte chunks
	(do ((i chunks (fx1- i))
	     (offset 0 (fx+ offset 64)))
	    ((fxzero? i))
	  (bytevector-copy! bv offset w64 0 64)  
	  (expand-chunk! w64)	     
	  (hash-and-update))                     
	
	;; do final partial chunk(s) 
	(bytevector-copy! bv (fx* chunks 64) w64 0 remainder)
	(bytevector-u8-set! w64 remainder #x80)  ;; 10000000 1-bit byte
	(if (fx< remainder 56)                  ;; enough room in this block for 8 byte bv length
	    (begin
	      (do ((i (fx1+ remainder) (fx1+ i))) ;; padd with 0s
		  ((fx= i 56))
		(bytevector-u8-set! w64 i 0))
	      (chunk-data-length! w64 len)
	      (expand-chunk! w64)
	      (hash-and-update))
	    (begin                                    ;; not enough room, do another block for bv length
	      (do ((i (fx1+ remainder) (fx1+ i))) ;; padd with 0s
		  ((fx= i 64))
		(bytevector-u8-set! w64 i 0))
	      (expand-chunk! w64)                 ;; process the partial 1-bit byte appended block first
	      (hash-and-update)
	      (bytevector-fill! w64 0)            ;; last block solely for the bv length in bits
	      (chunk-data-length! w64 len)
	      (expand-chunk! w64)
	      (hash-and-update)))))
    ;; return the final hash value 
    (let ((hc (make-bytevector 32 0)))
      (word-set! hc 0 h0)
      (word-set! hc 1 h1)
      (word-set! hc 2 h2)
      (word-set! hc 3 h3)
      (word-set! hc 4 h4)
      (word-set! hc 5 h5)
      (word-set! hc 6 h6)
      (word-set! hc 7 h7)
      hc)))

;;  (string->bytevector s (make-transcoder (utf-8-codec))))

;; SHA-256 hash a bytevector or string of data.
;; data: (or string? bytevector?)
(: sha256 ((U Bytes String) -> Bytes))
(define (sha256 data)
  (cond 
   ((bytevector? data) (sha256-bytes data))
   ((string? data) (sha256-bytes (str->bv data)))))

;;  (string->bytevector s (make-transcoder (utf-8-codec))))

;; ;; f7846f55cf23e14eebeab5b4e1550cad5b509e3348fbc4efa3a1413d393cb650 
;; (sha256 (mk-msg "message digest"))

;; ;; short 1 block no room for length
;; ;; 5e43c8704ac81f33d701c1ace046ba9f257062b4d17e78f3254cbf243177e4f2
;; (hexstr (sha256 (mk-msg "012345678901234567890123456789012345678901234567890123456789")))

;; ;; 2 blocks
;; ;; 57445fa40b08c60cdcba7ca39c756de614d11482e3f15a925f548688c19e58f4
;; (sha256 (mk-msg "0123456789012345678901234567890123456789012345678901234567890123456789"))


;; (define msg "GET\nwebservices.amazon.com\n/onca/xml\nAWSAccessKeyId=00000000000000000000&ItemId=0679722769&Operation=ItemLookup&ResponseGroup=ItemAttributes%2COffers%2CImages%2CReviews&Service=AWSECommerceService&Timestamp=2009-01-01T12%3A00%3A00Z&Version=2009-01-06")

;; (define key "1234567890")
;; (base64-encode (hmac-sha256 key msg))
;; "Nace+U3Az4OhN7tISqgs1vdLBHBEijWcBeCqL5xN9xg="
