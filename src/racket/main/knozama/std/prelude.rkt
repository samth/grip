#lang racket

(provide fx1+
	 fx1-
	 fxzero?
	 add1 sub1
	 begin0
	 cons*)

(require racket/fixnum)

(define-syntax fx1+
  (syntax-rules ()
    ((_ x)
     (fx+ x 1))))

(define-syntax fx1-
  (syntax-rules ()
    ((_ x)
     (fx- x 1))))

(define-syntax fxzero?
  (syntax-rules ()
    ((_ x)
     (fx= x 0))))

(define-syntax add1
  (syntax-rules ()
    ((add1 x)
     (+ 1 x))))

 (define-syntax sub1
   (syntax-rules ()
     ((sub1 x)
      (- x 1))))

(define-syntax begin0
  (syntax-rules ()
    ((begin0 e1 es ...)
     (let ((result e1))
       es ...
       result))))

(define (cons* a1 a2 . rest)
  (if (null? rest)
     (cons a1 a2)
     (cons a1 (apply cons* (cons a2 rest)))))
