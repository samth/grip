
(library 
 (rl3 crypto hash sha1)

 (export ashex sha1 sha1-stream hmac-sha1)

 (import
  (rnrs base)
  (only (rnrs bytevectors)
        bytevector?
        make-bytevector bytevector-length
        bytevector-copy! bytevector-fill!
        bytevector-u8-set! bytevector-u8-ref
        bytevector-u32-set! bytevector-u32-ref)
  (only (rnrs arithmetic fixnums)
        fx* fx+ fx- fx=? fx>? fx<? fx<=? fxzero?)
  (only (rnrs arithmetic bitwise)
        bitwise-ior bitwise-xor bitwise-and bitwise-arithmetic-shift-left
        bitwise-arithmetic-shift-right bitwise-not)
  (only (rnrs io simple)
        display newline)
  (only (rnrs io ports)
        get-bytevector-n!
        open-bytevector-input-port
        eof-object?)
  (only (rnrs control)
        do)
  (only (rnrs r5rs)
        quotient remainder)
  (only (rl3 env prelude)
        fx1+))

 (define H0 #x67452301)
 (define H1 #xefcdab89)
 (define H2 #x98badcfe)
 (define H3 #x10325476)
 (define H4 #xc3d2e1f0)

 ;; Remove as this temporary
 (define ashex
   (lambda (bv)
     (let ((len (bytevector-length bv)))
       (let loop ((i 0) (s ""))
	 (if (fx=? i len)
	    s
	    (loop (fx1+ i) (string-append s (number->string (bytevector-u8-ref bv i) 16))))))))

 (define block-size 64)
 
 (define w-size (* 80 4)) ;; 80 32-bit words.

 (define w32 #xffffffff)

 (define word-set!
   (lambda (bv32 idx val)
     (bytevector-u32-set! bv32 (fx* 4 idx) val 'big)))

 (define word-ref
   (lambda (bv32 idx)
     (bytevector-u32-ref bv32 (fx* 4 idx) 'big)))

 (define m32
   (lambda (x)
     (bitwise-and x w32)))

 (define m32+
   (lambda (x y)
     (m32 (+ x y))))
 
 (define circular-shift
   (lambda (x n)
     (m32 (bitwise-ior (bitwise-arithmetic-shift-left  x n)
                       (bitwise-arithmetic-shift-right x (- 32 n))))))
 
 (define f-k
   (lambda (i)
     (cond
      ((fx<=? i 19)
       (values (lambda (b c d)
                 (bitwise-ior (bitwise-and b c)
                              (bitwise-and (bitwise-not b) d)))
               #x5A827999))      
      ((fx<=? i 39)
       (values (lambda (b c d)
                 (bitwise-xor b c d))
               #x6ED9EBA1))      
      ((fx<=? i 59)
       (values (lambda (b c d)
                 (bitwise-ior (bitwise-and b c)
                              (bitwise-and b d)
                              (bitwise-and d c)))
               #x8F1BBCDC))
      (else
       (values (lambda (b c d)
                 (bitwise-xor b c d))
               #xCA62C1D6)))))

 ;; processed 64 bytes at a time with some remainder bytes
 ;; if enough room for the msg length but it in the block
 ;; else pad out this block, process it, then  pad + length
 ;; finishing block and process it.
 ;; msg : bytevector? (assert (fx=? (bytevector-length msg 64)))
 (define sha1-block-partial
   (let ((pad-sentinel #x80))  ;; 1000 000 per spec
     (lambda (block n h0 h1 h2 h3 h4 total)
       ;;(display "Partial-In")(newline)
       ;;(display block) (newline)
       (bytevector-u8-set! block n pad-sentinel)
       ;;(display "Partial-In-1Bit")(newline)
       ;;(display block) (newline)
       (if (fx>? n 55)  ;; not enough room
           (begin
             (do ((i (fx1+ n) (fx1+ i)))
                 ((fx=? i block-size))
               (bytevector-u8-set! block i 0))
             (let-values (((h0 h1 h2 h3 h4)(sha1-block block h0 h1 h2 h3 h4)))
               (sha1-block-partial (bytevector-fill! block 0) 0 h0 h1 h2 h3 h4 total)))
           (begin
             (do ((i (fx1+ n) (fx1+ i))) ;; pad out if necessary
                 ((fx=? i 56))
               (bytevector-u8-set! block i 0))
             (let ((bits (* 8 total)))
               (let ((high32 (quotient bits w32))
                     (low32  (remainder bits w32)))
                 (word-set! block 14 high32)
                 (word-set! block 15 low32)
                 ;;(display "Partial Padded")(newline)
                 ;;(display block)(newline)
                 (sha1-block block h0 h1 h2 h3 h4))))))))
 
 (define sha1-block
   (lambda (block h0 h1 h2 h3 h4)
     ;;(display "SHA-1 Block")(newline)
     ;;(display block)
     ;;(newline)
     (let ((w (make-bytevector w-size 0)))
       (do ((i 0 (fx1+ i)))
           ((fx=? i 16))
         (word-set! w i (word-ref block i)))
       (do ((i 16 (fx1+ i)))
           ((fx=? i 80))
         (word-set! w i (circular-shift (bitwise-xor (word-ref w (fx- i 3))
                                                     (word-ref w (fx- i 8))
                                                     (word-ref w (fx- i 14))
                                                     (word-ref w (fx- i 16)))
                                        1)))
       (let loop ((i 0) (a h0) (b h1) (c h2) (d h3) (e h4))
         (if (fx=? i 80)
             (values (m32+ h0 a) (m32+ h1 b) (m32+ h2 c) (m32+ h3 d) (m32+ h4 e))
             (let-values (((f k) (f-k i)))  ;; MASK FORCE TO 32 BITS????
               (let ((tmp (m32 (+ (circular-shift a 5) (f b c d) e k (word-ref w i)))))
                 (loop (fx1+ i) tmp a (circular-shift b 30) c d))))))))
 
 ;; (let ((e d) (d c) (c (circular-shift b 30)) (b a) (a tmp))
 (define sha1-stream
   (lambda (bip)
     (let ((block (make-bytevector block-size 0)))   ;; 512 bit buffer
       (let ((next-block (lambda ()
                           (get-bytevector-n! bip block 0 block-size))))
         (let loop ((n (next-block))
                    (h0 H0) (h1 H1) (h2 H2) (h3 H3) (h4 H4) (total 0))
           (if (or (eof-object? n) (fxzero? n))
               (let ((result (make-bytevector 20 0)))
                 (word-set! result 0 h0) (word-set! result 1 h1) (word-set! result 2 h2)
                 (word-set! result 3 h3) (word-set! result 4 h4)
                 result)
               (let ((total (+ total n)))
                 (if (fx=? n block-size)
                     (let-values (((h0 h1 h2 h3 h4) (sha1-block block h0 h1 h2 h3 h4)))
                       (loop (next-block) h0 h1 h2 h3 h4 total))
                     (let-values (((h0 h1 h2 h3 h4) (sha1-block-partial block n h0 h1 h2 h3 h4 total)))
                       (loop (next-block) h0 h1 h2 h3 h4 total))))))))))

 (define sha1
   (lambda (bytes)
     (assert (bytevector? bytes))
     (sha1-stream (open-bytevector-input-port bytes))))
 
 (define bytevector-append
   (lambda (bv1 bv2)
     (let ((len1 (bytevector-length bv1))
           (len2 (bytevector-length bv2)))
       (let ((bv (make-bytevector (fx+ len1 len2))))
         (bytevector-copy! bv1 0 bv 0 len1)
         (bytevector-copy! bv2 0 bv len1 len2)
         bv))))

 (define hmac-sha1
   (lambda (key msg)
     (let ((opad (make-bytevector block-size #x5c))
           (ipad (make-bytevector block-size #x36))
           (klen (bytevector-length key))
           (mlen (bytevector-length msg)))
       (let ((key  (if (fx<? klen block-size)
                       (let ((key-block (make-bytevector block-size 0)))
                         (bytevector-copy! key 0 key-block 0 klen)
                         key-block)
                       key)))
         (do ((i 0 (fx1+ i)))
             ((fx=? i klen))
           (bytevector-u8-set! ipad i (bitwise-xor (bytevector-u8-ref ipad i) (bytevector-u8-ref key i)))
           (bytevector-u8-set! opad i (bitwise-xor (bytevector-u8-ref opad i) (bytevector-u8-ref key i))))
         (sha1 (bytevector-append opad (sha1 (bytevector-append ipad msg))))))))
 )

;; (import (rl3 crypto hash sha1))

;; (debug-enable #f)

;; (ashex (sha1 (open-bytevector-input-port (string->utf8 "The quick brown fox jumps over the lazy dog"))))

;; (time (behex (sha1 (open-bytevector-input-port (string->utf8 "The quick brown fox jumps over the lazy cog")))))

;; (ashex (sha1 (string->utf8 "abc"))

;; (behex (sha1 (open-bytevector-input-port (string->utf8 "abc"))))

;; (ashex (sha1 (open-bytevector-input-port (string->utf8 ""))))

;; (ashex (sha1 (open-bytevector-input-port (string->utf8 "abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq"))))

;; (import (rl3 env prelude))7

;; (define behex
;;   (lambda (bv)
;;     (do ((i 0 (+ i 1)))
;;         ((= i 5)) 
;;       (display (number->string (bytevector-u32-ref bv (* i 4) 'big) 16)))))

;; (define ashex
;;   (lambda (bv)
;;     (let loop ((i 0) (s ""))
;;       (if (fx=? i 20)
;;           s
;;           (loop (fx1+ i) (string-append s (number->string (bytevector-u8-ref bv i) 16)))))))


;; (ashex (hmac-sha1 (string->utf8 "ray") (string->utf8 "cory")))

