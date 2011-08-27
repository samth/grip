#lang typed/racket/base

(provide fx1+ 
	 fx1-
	 fxzero?
	 fxdiv-and-mod
	 str->bv
	 hexstr)

(require racket/fixnum)

(require (only-in (planet knozama/r6rs:1/bytevectors)
		  bytevector-u8-ref
		  bytevector-length
		  string->utf8))

(define-syntax fx1+
  (syntax-rules ()
    ((fx1+ x)
     (fx+ 1 x))))

(define-syntax fx1-
  (syntax-rules ()
    ((fx1- x)
     (fx- x 1))))

(define fxzero? zero?)

(: fxdiv-and-mod (Fixnum Fixnum -> (values Fixnum Fixnum)))
(define (fxdiv-and-mod x y)
  (values (fxquotient x y)
	  (fxmodulo x y)))

;; UTF-8 String to Bytevector.
(: str->bv (String -> Bytes))
(define (str->bv s)
  (string->utf8 s))

(: mk-msg (String -> Bytes))
(define (mk-msg s)
    (string->utf8 s))

;; Bytevector to hex string
;; Only the first 8 32-bit words
(: hexstr (Bytes -> String))
(define (hexstr bv)
  (let ((hex (lambda: ((i : Integer))
	     (let ((s (number->string (bytevector-u8-ref bv i) 16)))
	       (if (fx= (string-length s) 1)
		  (string-append "0" s)
		  s))))
      (len (bytevector-length bv)))
    (do: : String ((i : Integer 0 (fx1+ i))
		   (s : String "" (string-append s (hex i))))
	 ((fx= i len) s))))
