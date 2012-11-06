#lang typed/racket/base
  
(require 
 racket/pretty
 "fptypes.rkt"
 (only-in "combo.rkt"
          generate-proper-subsets))
          
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; For each frequent itemset X,
;;  - for each proper nonempty subset A of X,
;; Then
;; Let B = X - A
;; A -> B is an association rule if
;;  Confidence (A -> B) >= MIN-CONF
;; Where
;; Support (A -> B) = Support (A U B) = Support (X)
;; Confidence (A -> B) = Support (A U B) / Support (A)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(: generate-rules (TxDB Integer -> Void))
(define (generate-rules tx-db support)
  (for-each (λ: ((tx : Tx))
              (let ((subsets (generate-proper-subsets (Tx-items tx))))
                (pretty-print subsets)
                (void)))
            tx-db))

