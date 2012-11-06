#lang typed/racket/base

(require
 "model.rkt"
 "factor.rkt"
 "factor-util.rkt")

(define M (new-model "Test" '(2 3 4)))
(define F (new-factor M '(A B C) '#(0.5 0.3 0.2)))

(define (test-cards)
  (factor-cardinalities M F))

(test-cards)

(index-factor F '(A))