#lang racket/base

(require test-engine/racket-tests)

(require 
 "utils.rkt"
 "model.rkt")

;(define M (new-model "Test" '(2 2 2)))

;(check-expect
; (sort-variables (model-variables M))
; '(A B C))
;
;(check-expect 
; (variable-cardinalities M '(B C))
; '(2 2))
;
;(check-expect
; (variable-definitions M)
; '((A . 2) (B . 2) (C . 2)))
;
;(test)
