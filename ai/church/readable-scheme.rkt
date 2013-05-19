#lang racket

;; KILL THIS FILE RPR


(provide 
        true false true? false?
        tagged-list?
        gensym)

(define pair cons)

(define (true? x)
  (not (eq? x false)))

(define (false? x)
  (eq? x false))

(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      false))
