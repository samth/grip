#lang racket

(require 
 rackunit
 rackunit/text-ui
 "model.rkt"
 "factor.rkt")

;(define M (new-model "Test" '(2 3 2)))
;(define F (new-factor M '(A B) '#(0.2 0.8 0.4 0.6 0.1 0.9)))
;
;(define-test-suite
;  Factor-tests
;  
;  (test-case 
;   "Factor operations tests"
;   
;   (define M (new-model "Test" '(2 2 2)))
;   (define F (new-factor M 
;		    '(A B C) 
;		    '#(0.2 0.8 0.4 0.6 0.1 0.3 0.5 0.7)))
;   
;   (check-equal?
;    (factor-ordinality F)
;    '(0 1 2))
;   
;   (check-equal?
;    (factor-cardinality M F)
;    '(2 2 2))))
;  
;(define-test-suite
;  Factor-*
;
;  (test-case
;   "Factor multiplication"
;
;   (define M (new-model "Test" '(2 2 2)))
;   (define F (new-factor M 
;		    '(A B C) 
;		    '#(0.2 0.8 0.4 0.6 0.1 0.3 0.5 0.7)))
;
;   
;   (check-equal? #t (f* M F F))))
;
;(define (test-it)
;  (run-tests (test-suite "All Factor Tests"
;			 Factor-tests
;			 Factor-*)))