#lang racket

(require 
 racket/flonum
 rackunit
 rackunit/text-ui
 "factor.rkt")

(define F1 (mkFactor '(A B) '(3 2) (flvector 0.5 0.8 0.1 0.0 0.3 0.9)))

(define F2 (mkFactor '(B C) '(2 2)
		     (flvector 0.5 0.7 0.1 0.2)))

(define F3 (mkFactor '(A B C) '(3 2 2)
		     (flvector 0.25 0.35 0.08 0.16 0.05  0.07 0.0 0.0 0.15 0.21 0.09 0.18)))

(pretty-print (f* F1 F2))

(define-test-suite
 Factor-tests
 
 (test-case 
  "Factor operations tests"
    
  (check-equal?
   (factor-ordinality F1)
   '(0 1 2))
  
  (check-equal?
   (factor-cardinality F1)
   '(2 2))))
 
(define-test-suite
 Factor-*

 (test-case
  "Factor multiplication"

  (check-equal? #t (f* F1 F2))))

(define (test-it)
  (run-tests (test-suite "All Factor Tests"
			 Factor-tests
			 Factor-*)))
