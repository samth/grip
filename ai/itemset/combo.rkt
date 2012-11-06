#lang typed/racket/base

(provide:
 (generate-proper-subsets [ItemSet -> (Listof ItemSet)])
 (combinations [ItemSet -> (Listof ItemSet)])
 (combinations-with [ItemSet ItemSet -> (Listof ItemSet)])
 (extend-base-with-combinations [ItemSet ItemSet -> (Listof ItemSet)]))
               
(require "fptypes.rkt")

(: generate-proper-subsets (ItemSet -> (Listof ItemSet)))
(define (generate-proper-subsets item-set)
  (define len (length item-set))
  (filter (λ: ((item-set : ItemSet))
            (and (pair? item-set)
                 (< (length item-set) len)))
            (combinations item-set)))

(: expand-combos [Item (Listof ItemSet) -> (Listof ItemSet)])
(define (expand-combos item combos) 
  (map (λ: ((is : ItemSet))
         (cons item is))                     
       combos))

(: combinations (ItemSet -> (Listof ItemSet)))
(define (combinations items)
  (if (null? items)
      '(())
      (let ((sub-combo (combinations (cdr items))))
        (append (expand-combos (car items) sub-combo) sub-combo))))

(: combinations-with (ItemSet ItemSet -> (Listof ItemSet)))
(define (combinations-with base with)
  (map (λ: ((with-combo : ItemSet))
         (append with-combo base))
       (combinations with)))

;; Does not include the base
(: extend-base-with-combinations (ItemSet ItemSet -> (Listof ItemSet)))
(define (extend-base-with-combinations base with)
  (if (null? with)
      (list base)
      (let: ((with-combos : (Listof ItemSet) (filter pair? (combinations with))))    
        (map (λ: ((with-combo : ItemSet))
               (append with-combo base))
             with-combos))))
       
                                   
