#lang racket

(provide mem list-index repeat)

;; HACK in SOMETHING for now RPR
;; memoize
(define-syntax mem
  (syntax-rules ()
    [(_ (f args ...))
     (f args ...)]))

(define (list-index pred lst . i)
  (if (null? lst)
      false
      (let ((i (if (null? i) 0 (first i))))
        (if (pred (first lst))
            i
            (list-index pred (rest lst) (+ i 1))))))

(define (repeat n thunk)
  (if (> n 0)
      (cons (thunk) (repeat (- n 1) thunk))
      (list) ))