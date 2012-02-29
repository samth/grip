#lang typed/racket/base

(provide hmac-sha256 hmac-sha1)

(require (only-in (planet rpr/r6rs:1/bytevectors)
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
		  bytevector-u8-set!))

(require racket/fixnum
	 "hash/sha256.rkt"
	 "private/util.rkt"
	 (only-in "hash/sha1.rkt"
		  sha1-bytes)
	 (only-in "private/util.rkt"
		  fxdiv-and-mod fx1+ fx1- fxzero? str->bv))

(: block-size Integer)
(define block-size 64)

(: bytevector-append (Bytes Bytes -> Bytes))
(define (bytevector-append bv1 bv2)
  (let ((len1 (bytevector-length bv1))
      (len2 (bytevector-length bv2)))
    (let ((bv (make-bytevector (fx+ len1 len2) 0)))
      (bytevector-copy! bv1 0 bv 0 len1)
      (bytevector-copy! bv2 0 bv len1 len2)
      bv)))

(: hmac (Bytes Bytes (Bytes -> Bytes) -> Bytes))
(define (hmac key msg hash)
  (let ((klen (bytevector-length key))
      (mlen (bytevector-length msg))
      (opad (make-bytevector block-size #x5c))
      (ipad (make-bytevector block-size #x36)))
    (let ((key (if (fx< klen block-size)
		(let ((key-block (make-bytevector block-size 0)))
		  (bytevector-copy! key 0 key-block 0 klen)
		  key-block)
		(hash key))))
      (do ((i 0 (fx1+ i)))
	  ((fx= i klen))
	(bytevector-u8-set! ipad i (bitwise-xor (bytes-ref ipad i) (bytes-ref key i)))
	(bytevector-u8-set! opad i (bitwise-xor (bytes-ref opad i) (bytes-ref key i))))
      (hash (bytevector-append opad (hash (bytevector-append ipad msg)))))))

(: as-bytes ((U Bytes String) -> Bytes))
(define (as-bytes value)
  (if (string? value)
     (str->bv value)
     value))

(: hmac-sha256 ((U Bytes String) (U Bytes String) -> Bytes))
(define (hmac-sha256 key msg)
  (let ((k (as-bytes key))
      (m (as-bytes msg)))
    (hmac k m sha256-bytes)))

(: hmac-sha1 ((U Bytes String) (U Bytes String) -> Bytes))
(define (hmac-sha1 key msg)
  
  (let ((k (as-bytes key))
      (m (as-bytes msg)))
    (hmac k m sha1-bytes)))
