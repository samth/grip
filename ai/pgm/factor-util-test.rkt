#lang racket

(require test-engine/racket-tests)

(require 
 racket/pretty
 "model.rkt"
 "factor.rkt")

;;"factor-util.rkt")

;(define M (new-model "Test" '(2 2 2)))
;(define F (new-factor M '(A B C) '#(0.2 0.8 0.4 0.6 0.1 0.3 0.5 0.7)))

;(check-expect
;(index-factor (factor-cardinality M F) F '(C B))
;#f)

;(check-expect
; (for/parameters '(2 2) 
;                 (λ (v vidx)
;                   (display (format "~s ~s" v vidx))))
; #f)

(test)