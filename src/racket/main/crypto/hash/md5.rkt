#lang r6rs

(provide md5)

(require
 (rnrs base)
 (only (r5rs)
       remainder quotient modulo
       inexact->exact)
 (only (rnrs control)
       do when)
 (only (rnrs io simple)
       display newline)
 (only (rnrs io ports)
       get-bytevector-n!
       eof-object?)
 (rnrs arithmetic fixnums)
 (only (rnrs bytevectors)
       make-bytevector bytevector-fill! bytevector-length
       bytevector-u32-set! bytevector-u32-ref
       bytevector-u8-set! bytevector-u8-ref
       bytevector-u64-set!)
 (only (rnrs arithmetic bitwise)
       bitwise-and bitwise-ior bitwise-xor bitwise-not
       bitwise-arithmetic-shift-left bitwise-arithmetic-shift-right)
 (only (rl3 env prelude)
       fx1+))

(define ashex
  (lambda (bv)
    (let ((len (bytevector-length bv)))
      (let loop ((i 0) (s ""))
	(if (fx=? i len)
	   s
	   (loop (fx1+ i) (string-append s (number->string (bytevector-u8-ref bv i) 16))))))))

(define w-size (fx* 16 4)) ;; 16 32-bit words

(define block-size 64)

(define w32 #xffffffff)

(define word-set!
  (lambda (bv32 idx val)
    (bytevector-u32-set! bv32 (fx* 4 idx) val 'little)))

(define word-ref
  (lambda (bv32 idx)
    (bytevector-u32-ref bv32 (fx* 4 idx) 'little)))

(define m32
  (lambda (x)
    (bitwise-and x #xffffffff)))

(define m32+
  (lambda (x y)
    (m32 (+ x y))))

(define r '#(7 12 17 22  7 12 17 22  7 12 17 22  7 12 17 22
	  5  9 14 20  5  9 14 20  5  9 14 20  5  9 14 20
	  4 11 16 23  4 11 16 23  4 11 16 23  4 11 16 23
	  6 10 15 21  6 10 15 21  6 10 15 21  6 10 15 21))

(define k
  '#(3614090360 3905402710 606105819 3250441966 4118548399
		1200080426 2821735955 4249261313 1770035416 2336552879
		4294925233 2304563134 1804603682 4254626195 2792965006
		1236535329 4129170786 3225465664 643717713 3921069994
		3593408605 38016083 3634488961 3889429448 568446438
		3275163606 4107603335 1163531501 2850285829 4243563512
		1735328473 2368359562 4294588738 2272392833 1839030562
		4259657740 2763975236 1272893353 4139469664 3200236656
		681279174 3936430074 3572445317 76029189 3654602809
		3873151461 530742520 3299628645 4096336452 1126891415
		2878612391 4237533241 1700485571 2399980690 4293915773
		2240044497 1873313359 4264355552 2734768916 1309151649
		4149444226 3174756917 718787259 3951481745) )

(define H0 #x67452301)
(define H1 #xefcdab89)
(define H2 #x98badcfe)
(define H3 #x10325476)

(define pad-sentinel #x80)

(define circular-shift
  (lambda (x n)
    (m32 (bitwise-ior (bitwise-arithmetic-shift-left  x n)
		      (bitwise-arithmetic-shift-right x (fx- 32 n))))))

(define md5-block-partial
  (lambda (block n h0 h1 h2 h3 total sentinel?)
    (when sentinel?
      (bytevector-u8-set! block n pad-sentinel))
    (if (fx>? n 55) ;; not enough room for bit length of msg (8 bytes)
       (begin
	 (do ((i (fx1+ n) (fx1+ i)))
	     ((fx=? i block-size))
	   (bytevector-u8-set! block i 0))
	 (let-values (((h0 h1 h2 h3) (md5-block block h0 h1 h2 h3)))
	   (md5-block-partial (bytevector-fill! block 0) h0 h1 h2 h3 total #f)))
       (begin
	 (when sentinel?  ;; padd out with 0
	   (do ((i (fx1+ n) (fx1+ i)))
	       ((fx=? i 56))
	     (bytevector-u8-set! block i 0)))
	 ;;(display "Bits: ") (display (* 8 total)) (display "  ") (display total)(newline)
	 (bytevector-u64-set! block (* 4 14) (* 8 total) 'little)
	 (md5-block block h0 h1 h2 h3)))))

(define md5-block
  (lambda (block h0 h1 h2 h3)
    ;;(display "MD5 block")  (newline)
    ;;(display block)(newline)
    ;;(display (ashex block))(newline)
    ;; main looop
    (let loop ((i 0) (a h0) (b h1) (c h2) (d h3))
      (if (fx=? i 64)
	 (values (m32+ a h0) (m32+ b h1) (m32+ c h2) (m32+ d h3))
	 (let-values (((f g) (cond
			    ((fx<=? i 15)
			     (values (lambda (b c d)
				       (bitwise-xor d (bitwise-and b (bitwise-xor c d))))
				     i))
			    ((fx<=? i 31)
			     (values (lambda (b c d)
				       (bitwise-xor c (bitwise-and d (bitwise-xor b c))))
				     (modulo (+ (* 5 i) 1) 16)))
			    ((fx<=? i 47)
			     (values (lambda (b c d)
				       (bitwise-xor b c d))
				     (modulo (+ (* 3 i) 5) 16)))
			    ((fx<=? i 63)
			     (values (lambda (b c d)
				       (bitwise-xor c (bitwise-ior b (bitwise-not d))))
				     (modulo (* 7 i) 16))))))
	   (loop (fx1+ i) d (m32 (+ b (circular-shift (m32 (+ a (f b c d)
							      (vector-ref k i)
							      (word-ref block g)))
						      (vector-ref r i))))
		 b c))))))

(define md5
  (lambda (bip)
    (let ((block (make-bytevector block-size 0))) ;; 512 bit block
      (let ((next-block (lambda ()
			(get-bytevector-n! bip block 0 block-size))))
	(let loop ((n (next-block)) (h0 H0) (h1 H1) (h2 H2) (h3 H3) (total 0))
	  ;;(display "Next block: ") (display n) (newline)
	  ;;(display block)(newline)
	  (if (or (eof-object? n) (fxzero? n) (fx<? n block-size)) ;; Larceny bug requires fxzero? check
	     (let-values (((h0 h1 h2 h3) (md5-block-partial block n h0 h1 h2 h3 (+ n total) #t)))
	       (let ((result (make-bytevector 16)))
		 (word-set! result 0 h0) (word-set! result 1 h1)
		 (word-set! result 2 h2) (word-set! result 3 h3)
		 result))
	     (let-values (((h0 h1 h2 h3) (md5-block block h0 h1 h2 h3)))
	       (loop (next-block) h0 h1 h2 h3 (+ n total)))))))))



;; (import (rl3 crypto hash md5))

;; (define sl "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa")
;; (string-length sl)
;; (ashex (md5 (open-bytevector-input-port (string->utf8 "The quick brown fox jumps over the lazy dog"))))
;; (ashex (md5 (open-bytevector-input-port (string->utf8 "abc"))))

;; (import (rl3 env debug))
;; (debug-enable #f)


;; (define ashex
;;   (lambda (bv)
;;     (let loop ((i 0) (s ""))
;;       (if (fx=? i 16)
;;           s
;;           (loop (fx1+ i) (string-append s (number->string (bytevector-u8-ref bv i) 16)))))))


