#lang typed/racket/base

(provide 
 =>)
 ;; begin0)

;;	 cons*)

;; (define-syntax add1
;;   (syntax-rules ()
;;     ((add1 x)
;;      (+ 1 x))))

;;  (define-syntax sub1
;;    (syntax-rules ()
;;      ((sub1 x)
;;       (- x 1))))

;; (define-syntax begin0
;;   (syntax-rules ()
;;     ((begin0 e1 es ...)
;;      (let ((result e1))
;;        es ...
;;        result))))

(define-syntax =>
  (syntax-rules ()
    ((_ exp ...)
     (lambda ()
       exp ...))))

;; (: cons* (All (a) a a * -> (Listof a)))
;; (define (cons* a1 a2 . rest)
;;   (if (null? rest)
;;      (cons a1 a2)
;;      (cons a1 (apply cons* (cons a2 rest)))))
