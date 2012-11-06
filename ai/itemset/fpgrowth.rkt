#lang typed/racket/base

(require 
 racket/pretty
 racket/match
 "fptypes.rkt"
 (only-in "combo.rkt"
          extend-base-with-combinations)
 "fptest.rkt"
 (only-in "fptree.rkt"
          is-root?
          build-FPTree))

(: has-single-path? (FPTree -> Boolean))
(define (has-single-path? fptree)  
  (: only-one-child (FPTree -> Boolean))
  (define (only-one-child fptree)
    (let ((kids (FPTree-children fptree)))
      (cond 
        ((null? kids) #t)
        ((equal? (length kids) 1)          
         (only-one-child (car kids)))
        (else #f))))  
  (only-one-child fptree))

(: emit-pattern-csv (ItemSet Integer -> Void))
(define (emit-pattern-csv items support)  
  (display (number->string support))
  (for-each (λ: ((item : Item))
              (display ",")
              (display item))            
            (sort items char<?))
  (displayln ""))

(: emit-combos (FP-Tree ItemSet Integer -> Void))
(define (emit-combos fp-tree conditional-set support)    
  (for-each (λ: ((combo : ItemSet))
              (emit-pattern-csv combo support))
            (ann (filter pair? (extend-base-with-combinations conditional-set 
                                                              (collect-single-path-pattern (FP-Tree-root fp-tree))))
                 (Listof ItemSet))))
              
(: emit-base-support (ItemSet Integer -> Void))
(define (emit-base-support item-set base-support)   
  (emit-pattern-csv item-set base-support))

(: collect-single-path-pattern (FPTree -> ItemSet))
(define (collect-single-path-pattern fptree)          
  
  (: walk-down (FPTree ItemSet -> ItemSet))
  (define (walk-down fptree item-set)
    (let ((kids (FPTree-children fptree)))
      (if (null? kids)
          (reverse (cons (Node-item (FPTree-node fptree)) item-set))          
          (walk-down (car kids) (cons (Node-item (FPTree-node fptree)) item-set)))))
  
  (define: kids : (Listof FPTree) (FPTree-children fptree))    
  
  (if (null? kids)
      kids
      (walk-down (car kids) '())))

(: walk-up-form-base-path (FPTree -> Tx))
(define (walk-up-form-base-path fptree)
  (let: loop : Tx ((sub-tree : FPTree fptree) (items : ItemSet '()))
    (if (is-root? sub-tree)
        (Tx (cdr (reverse items)) (Node-support (FPTree-node fptree))) ;; cdr drops the conditioning item. (c b a) -> (b a)
        (loop (assert (FPTree-parent sub-tree))
              (cons (Node-item (FPTree-node sub-tree)) items)))))

(: header-support ((Listof FPTree) -> Integer))
(define (header-support bases)
  (define: (support-sum (fptree : FPTree) (sum : Integer)) : Integer
    (+ (Node-support (FPTree-node fptree)) sum))
  (foldl support-sum 0 bases))

(: header-conditional-pattern-base (Header -> TxDB))
(define (header-conditional-pattern-base header)
  (map walk-up-form-base-path (Header-bases header)))

(: extend-condition-set (Header ItemSet -> ItemSet))
(define (extend-condition-set header condition-set)
  (cons (Header-item header) condition-set))

(: header-base-support (Header -> Integer))
(define (header-base-support header)
  (header-support (Header-bases header)))

(: collect-combos (FP-Tree ItemSet Integer -> TxDB))
(define (collect-combos fp-tree conditional-set support)
  '())

(: fp-growth (FP-Tree ItemSet Integer -> Void))
(define (fp-growth fp-tree condition-set support)      
  (define headers (FP-Tree-headers fp-tree))      
  (if (has-single-path? (FP-Tree-root fp-tree))                 
      (when (pair? headers)
          (let* ((header (car headers))              
                 (base-support (header-base-support header)))
            (when (>= base-support support)              
                (emit-combos (build-FPTree (header-conditional-pattern-base header) base-support) 
                             (extend-condition-set header condition-set) base-support))))
      ;                (append (collect-combos (build-FPTree (header-conditional-pattern-base header) base-support)
      ;                                        (extend-condition-set header condition-set)
      ;                                        base-support)
      ;                                        tx-db)                
      (for-each (λ: ((header : Header))                  
                  (let ((base-support (header-base-support header))
                        (condition-set (extend-condition-set header condition-set)))
                    (when (>= base-support support)     
                      (emit-pattern-csv condition-set base-support)
                      (fp-growth (build-FPTree (header-conditional-pattern-base header) support)
                                 condition-set
                                 support))))                                 
                headers)))

(: test (-> Void))
(define (test)  
  (define support 2)
  (define tree (build-FPTree test-db-scratch support))
  (pretty-print test-db-scratch)
  (fp-growth tree '() support))