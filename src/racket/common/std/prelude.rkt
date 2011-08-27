#lang typed/racket/base

(provide 
 %%
 ==>)

(require racket/pretty)

;; create a procedure thunk out of a exp(s)
(define-syntax ==>
  (syntax-rules ()
    ((_ exp ...)
     (lambda ()
       exp ...))))

(define-syntax %%
  (syntax-rules ()
    ((_ exp)
     (let ((tmp exp))
       (pretty-print tmp)
       tmp))))

;; (: cons* (All (a) a a * -> (Listof a)))
;; (define (cons* a1 a2 . rest)
;;   (if (null? rest)
;;      (cons a1 a2)
;;      (cons a1 (apply cons* (cons a2 rest)))))
