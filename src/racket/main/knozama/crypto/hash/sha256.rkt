#lang racket

(provide sha256 hexstr)

(provide/contract (hmac-sha256 (-> (or/c string? bytevector?) (or/c string? bytevector?) bytevector?)))

(require (only-in knozama/std/prelude
		  fx1+ fx1-)
	 rnrs/bytevectors-6
	 rnrs/arithmetic/fixnums-6
	 rnrs/arithmetic/bitwise-6
	 (only-in rnrs/io/ports-6
		  make-transcoder
		  utf-8-codec
		  string->bytevector))

;; (require (rnrs base) 
;; 	 (only (rnrs bytevectors)
;; 	       bytevector? 
;; 	       make-bytevector bytevector-length
;; 	       bytevector-copy! bytevector-fill!
;; 	       bytevector-u8-set! bytevector-u8-ref
;; 	       bytevector-u32-set! bytevector-u32-ref)
;; 	 (only (rnrs arithmetic fixnums)
;; 	       fxzero? fx=?? fx* fx+ fx- fx<? fx>? fxmod fx<=? fxdiv-and-mod)
;; 	 (only (rnrs arithmetic bitwise)
;; 	       bitwise-xor bitwise-ior bitwise-and bitwise-not 
;; 	       bitwise-arithmetic-shift-right bitwise-arithmetic-shift-left)
;; 	 (only (rnrs control)
;; 	       do)
;; 	 (only (rnrs io ports)
;; 	       make-transcoder string->bytevector utf-8-codec)
;; 	 (only (rl3 env prelude)
;; 	       fx1+ fx1-))

 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; SHA-256 (SHA2) from FIPS PUB 180-3
;; Works with a in-memory bytevector of data.
;; i.e., no support for a byte stream which could be easily added.
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Bytevector to hex string
;; Only the first 8 32-bit words
(define hexstr
  (lambda (bv)
    (let ((hex (lambda (i)
	       (let ((s (number->string (bytevector-u8-ref bv i) 16)))
		 (if (fx=? (string-length s) 1)
		    (string-append "0" s)
		    s))))
	(len (bytevector-length bv)))
      (do ((i 0 (fx1+ i))
	   (s "" (string-append s (hex i))))
	  ((fx=? i len) s)))))

;; Mask for module 32 arithmetic
(define w32 #xffffffff)

;; truncate an integer to module 32 value.
(define m32 
  (lambda (x)
    (bitwise-and x w32)))

;; Add to int values module 32 arithmetic
(define m32+
  (lambda (x y)
    (m32 (+ x y))))

;; set the ith u32 location to the  u32 value 
(define word-set!
  (lambda (bv32 idx val)
    (bytevector-u32-set! bv32 (fx* 4 idx) val 'big)))

;; the value of the ith u32 location
(define word-ref
  (lambda (bv32 idx)
    (bytevector-u32-ref bv32 (fx* 4 idx) 'big)))

;; SHA-256 right rotate 
(define rotate
  (lambda (v n)
    (m32 (bitwise-ior (bitwise-arithmetic-shift-right v n)
		      (bitwise-arithmetic-shift-left v (fx- 32 n))))))

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
(define k
  (do ((constants (make-bytevector (fx* 64 4)))
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
      ((fx=? idx 64) constants)
    (word-set! constants idx (car round-values))))

;; Expand a 64 byte chunk
(define expand-chunk!
  (lambda (w64)
    (do ((i 16 (fx1+ i)))
	((fx=? i 64))
      (let ((s0 (let ((w (word-ref w64 (fx- i 15))))
		  (bitwise-xor (rotate w 7)
			       (rotate w 18)
			       (bitwise-arithmetic-shift-right w 3))))
	    (s1 (let ((w (word-ref w64 (fx- i 2))))
		  (bitwise-xor (rotate w 17)
			       (rotate w 19)
			       (bitwise-arithmetic-shift-right w 10)))))
	(word-set! w64 i (m32+ (word-ref w64 (fx- i 16))
			       (m32+ s0 (m32+ (word-ref w64 (fx- i 7)) s1))))))))

;; set the data length in # of bits in the chunk
(define chunk-data-length!
  (lambda (w64 len)
    (let ((bits (* len 8)))
      (let ((low (m32 bits))
	    (high (m32 (bitwise-arithmetic-shift-right bits 32))))
	(word-set! w64 14 high)
	(word-set! w64 15 low)))))

(define hash 
  (lambda (bv)
    (let ((len (bytevector-length bv))  ;; size of data to hash
	  (w64 (make-bytevector 256 0)) ;; working buffer 64 32-bit words (256 bytes)
	  (h0 H0)(h1 H1)(h2 H2)(h3 H3)(h4 H4)(h5 H5)(h6 H6)(h7 H7)) ;; init the hash
      
      ;; process all the 64 bytes (512 bit) chunks available
      (let-values (((chunks remainder)(fxdiv-and-mod len 64))) ;; # of 64 byte chunks and remainder
	(let ((hash-and-update 	       ;; Hash the chunk and update running hash values
	       (lambda ()
		 (let ((a h0)(b h1)(c h2)(d h3)(e h4)(f h5)(g h6)(h h7))
		   (do ((i 0 (fx1+ i)))
		       ((fx=? i 64))
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
	  (if (fx<=? remainder 56)                  ;; enough room in this block for 8 byte bv length
	      (begin
		(do ((i (fx1+ remainder) (fx1+ i))) ;; padd with 0s
		    ((fx=? i 56))
		  (bytevector-u8-set! w64 i 0))
		(chunk-data-length! w64 len)
		(expand-chunk! w64)
		(hash-and-update))
	      (begin                                    ;; not enough room, do another block for bv length
		(do ((i (fx1+ remainder) (fx1+ i))) ;; padd with 0s
		    ((fx=? i 64))
		  (bytevector-u8-set! w64 i 0))
		(expand-chunk! w64)                 ;; process the partial 1-bit byte appended block first
		(hash-and-update)
		(bytevector-fill! w64 0)            ;; last block solely for the bv length in bits
		(chunk-data-length! w64 len)
		(expand-chunk! w64)
		(hash-and-update)))))
      ;; return the final hash value 
      (let ((hc (make-bytevector 32)))
	(word-set! hc 0 h0)
	(word-set! hc 1 h1)
	(word-set! hc 2 h2)
	(word-set! hc 3 h3)
	(word-set! hc 4 h4)
	(word-set! hc 5 h5)
	(word-set! hc 6 h6)
	(word-set! hc 7 h7)
	hc))))

;; UTF-8 String to Bytevector.
(define s->bv
  (lambda (s)
    (string->bytevector s (make-transcoder (utf-8-codec)))))

;; SHA-256 hash a bytevector or string of data.
;; data: (or string? bytevector?)
(define sha256
  (lambda (data)
    (cond 
     ((bytevector? data) (hash data))
     ((string? data) (hash (s->bv data)))
     (else (error 'sha256 "String or Bytevector arg required.")))))

(define bytevector-append
  (lambda (bv1 bv2)
    (let ((len1 (bytevector-length bv1))
	(len2 (bytevector-length bv2)))
      (let ((bv (make-bytevector (fx+ len1 len2))))
	(bytevector-copy! bv1 0 bv 0 len1)
	(bytevector-copy! bv2 0 bv len1 len2)
	bv))))

(define block-size 64)

(define hmac
  (lambda (key msg)
    (let ((klen (bytevector-length key))
	(mlen (bytevector-length msg))
	(opad (make-bytevector block-size #x5c))
	(ipad (make-bytevector block-size #x36)))
      (let ((key (if (fx<? klen block-size)
		  (let ((key-block (make-bytevector block-size 0)))
		    (bytevector-copy! key 0 key-block 0 klen)
		    key-block)
		  (hash key))))
	(do ((i 0 (fx1+ i)))
	    ((fx=? i klen))
	  (bytevector-u8-set! ipad i (bitwise-xor (bytes-ref ipad i) (bytes-ref key i)))
	  (bytevector-u8-set! opad i (bitwise-xor (bytes-ref opad i) (bytes-ref key i))))
	(hash (bytevector-append opad (hash (bytevector-append ipad msg))))))))

(define hmac-sha256
  (lambda (key msg)
    (let ((k (if (string? key)
	      (s->bv key)
	      key))
	(m (if (string? msg)
	      (s->bv msg)
	      msg)))
      ;;(assert (bytevector? k))
      ;;(assert (bytevector? m))
      (hmac k m))))



;; (define mk-msg
;;   (lambda (s)
;;     (string->bytevector s (make-transcoder (utf-8-codec)))))

;; ;; f7846f55cf23e14eebeab5b4e1550cad5b509e3348fbc4efa3a1413d393cb650 
;; (sha256 (mk-msg "message digest"))

;; ;; short 1 block no room for length
;; ;; 5e43c8704ac81f33d701c1ace046ba9f257062b4d17e78f3254cbf243177e4f2
;; (sha256 (mk-msg "012345678901234567890123456789012345678901234567890123456789"))

;; ;; 2 blocks
;; ;; 57445fa40b08c60cdcba7ca39c756de614d11482e3f15a925f548688c19e58f4
;; (sha256 (mk-msg "0123456789012345678901234567890123456789012345678901234567890123456789"))


;; (define msg "GET\nwebservices.amazon.com\n/onca/xml\nAWSAccessKeyId=00000000000000000000&ItemId=0679722769&Operation=ItemLookup&ResponseGroup=ItemAttributes%2COffers%2CImages%2CReviews&Service=AWSECommerceService&Timestamp=2009-01-01T12%3A00%3A00Z&Version=2009-01-06")

;; (define key "1234567890")
;; (base64-encode (hmac-sha256 key msg))
;; "Nace+U3Az4OhN7tISqgs1vdLBHBEijWcBeCqL5xN9xg="