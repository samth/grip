#lang typed/racket/base

(provide:
 [by-freq-supported-txs (F-Map TxDB -> TxDB)]
 [create-f-map (TxDB Integer -> F-Map)]
 [FMap->FList (F-Map -> F-List)])

(require "fptypes.rkt")

(: create-f-map (TxDB Integer -> F-Map))
(define (create-f-map db support)
  
  (: item-support (HashTable Char Integer))
  (define item-support (make-hash))
  
  (: add-tx ((Listof Char) Integer -> Void))
  (define (add-tx tx count)
    (for-each (λ: ((item : Char))
                (hash-update! item-support 
                              item 
                              (λ: ((curr-count : Integer))
                                (+ count curr-count))
                              (λ () 0)))
              tx))
  
  (for-each (λ: ((tx : Tx)) (add-tx (Tx-items tx) (Tx-count tx))) db)
  
  (: fmap F-Map)
  (define fmap (make-hash))
  
  (let: ((item-cnts : (Listof (Pair Item Integer))
                    (sort (filter (λ: ((itemcnt : (Pair Item Integer)))
                                    (>= (cdr itemcnt) support))
                                  (hash->list item-support))
                          (λ: ((ic1 : (Pair Item Integer)) (ic2 : (Pair Item Integer)))
                            (if (> (cdr ic1) (cdr ic2))
                                #t
                                (if (equal? (cdr ic1) (cdr ic2))
                                    (char<? (car ic1) (car ic2))
                                    #f))))))
    (do ([item-cnts item-cnts (cdr item-cnts)]
         [ordinal 0 (add1 ordinal)])
      ([null? item-cnts] fmap)
      (let ((item-cnt (car item-cnts)))
        (hash-set! fmap (car item-cnt) (ItemData (cdr item-cnt) ordinal))))))

(: item-compare (F-Map Item Item -> Integer))
(define (item-compare fmap i1 i2) 
  (let ((ord1 (ItemData-ordinal (hash-ref fmap i1)))
        (ord2 (ItemData-ordinal (hash-ref fmap i2))))
    (- ord1 ord2)))

(: FMap->FList (F-Map -> F-List))
(define (FMap->FList fmap)
  (sort (hash-keys fmap)
        (λ: ((i1 : Item) (i2 : Item))
          (< (item-compare fmap i1 i2) 0))))

;; given a Tx retain supported items
;; sort in freq (support) order descending 
(: by-freq-supported-tx (F-Map Tx -> Tx))
(define (by-freq-supported-tx fmap tx)
  (let: ((items : ItemSet (Tx-items tx)))
    (Tx (sort (filter (λ: ((item : Item)) 
                    (hash-has-key? fmap item))
                  items)
          (λ: ((i1 : Item) (i2 : Item))
            (< (item-compare fmap i1 i2) 0)))
        (Tx-count tx))))

(: by-freq-supported-txs (F-Map TxDB -> TxDB))
(define (by-freq-supported-txs fmap txs)
  (map (λ: ((tx : Tx))
         (by-freq-supported-tx fmap tx))
       txs))
