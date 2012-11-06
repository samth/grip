#lang typed/racket/base

(provide: 
 [build-FPTree (TxDB Integer -> FP-Tree)]
 [is-root? (FPTree -> Boolean)])

(require "fptypes.rkt"
         "fppass1.rkt")
         
(: TOP-NODE Node)
(define TOP-NODE (Node #\null 0))

(: make-FP-Root (-> FPTree))
(define (make-FP-Root)
  (let ((this (FPTree TOP-NODE #f '())))
    (set-FPTree-parent! this this)
    this))
           
(: is-root? (FPTree -> Boolean))
(define (is-root? fptree)
  (eq? (FPTree-parent fptree) fptree))

(: make-empty-header-table (-> HeaderTable))
(define (make-empty-header-table) (make-hash '()))

(: item-child (FPTree Item -> (Option FPTree)))
(define (item-child fptree item)
  (let: loop : (Option FPTree) ((kids : (Listof FPTree) (FPTree-children fptree)))
    (if (null? kids)
        #f
        (if (char=? item (Node-item (FPTree-node (car kids))))
            (car kids)
            (loop (cdr kids))))))
  
(: inc-node-support! (FPTree Integer -> Void))
(define (inc-node-support! fptree cnt)
  (let ((node (FPTree-node fptree)))
    (set-Node-support! node (+ cnt (Node-support node)))))
  
(: add-child-to-fptree (FPTree FPTree -> Void))
(define (add-child-to-fptree fptree-parent fptree-child)
  (set-FPTree-children! fptree-parent (cons fptree-child (FPTree-children fptree-parent))))

(: add-node-to-header-table (HeaderTable FPTree -> Void))
(define (add-node-to-header-table header-table fptree)
  (let ((k (Node-item (FPTree-node fptree))))
    (hash-update! header-table k 
                  (λ: ((trees : (Listof FPTree)))
                    (cons fptree trees))
                  (λ () '()))))

(: add-node-to-tree (FPTree Item Integer -> FPTree))
(define (add-node-to-tree fptree item count) 
  (let ((kid (FPTree (Node item count) fptree '())))
    (add-child-to-fptree fptree kid)
    kid))

(: add-tx (HeaderTable FPTree Tx -> Void))
(define (add-tx header-table fptree tx)
  
  (define count (Tx-count tx))
  
  (: add-tx-items (FPTree ItemSet -> Void))
  (define (add-tx-items fptree tx)
    (if (null? tx)
        (void)
        (let ((item (car tx)))
          (let ((kid-fptree (item-child fptree item)))
          (if kid-fptree
              (begin                   
                (inc-node-support! kid-fptree count)
                (add-tx-items kid-fptree (cdr tx)))
              (let ((kid-fptree (add-node-to-tree fptree item count)))                
                (add-node-to-header-table header-table kid-fptree)
                (add-tx-items kid-fptree (cdr tx))))))))
  
  (add-tx-items fptree (Tx-items tx)))
  
(: HeaderTable->HeaderList (F-Map HeaderTable -> HeaderList))
(define (HeaderTable->HeaderList fmap header-table)
  (reverse (map (λ: ((item : Item))
                  (Header item (hash-ref header-table item)))
                (FMap->FList fmap))))
  
(require racket/pretty)

(: build-FPTree (TxDB Integer -> FP-Tree))
(define (build-FPTree tx-db support)
  ;; filter and order all txs
  (define fmap (create-f-map tx-db support))  
  (define db (by-freq-supported-txs fmap tx-db))
  (define root (make-FP-Root))  
  (define header-table (make-empty-header-table))
  (for-each (λ: ((tx : Tx))
              (add-tx header-table root tx))
            db)
  (FP-Tree root (HeaderTable->HeaderList fmap header-table)))
