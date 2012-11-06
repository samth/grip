#lang typed/racket/base

(provide TxDB Item 
         F-Map F-List
         ItemSet
         (struct-out Tx)
         (struct-out ItemData)
         (struct-out FPTree)
         (struct-out FP-Tree)
         (struct-out Node)
         (struct-out Header)
         HeaderList
         HeaderTable)

(struct: ItemData ([support : Integer] [ordinal : Integer]) #:transparent)
(struct: Tx ([items : ItemSet][count : Integer]) #:transparent)

(define-type ItemSet (Listof Item))
(define-type Item Char)
(define-type TxDB (Listof Tx))
(define-type F-Map (HashTable Item ItemData))
(define-type F-List (Listof Item))

(struct: Node ([item : Item] [support : Integer]) #:mutable #:transparent)

(define-type HeaderTable (HashTable Item (Listof FPTree)))

(struct: Header ([item : Item] [bases : (Listof FPTree)]) #:transparent)
(define-type HeaderList (Listof Header))

(struct: FPTree ([node : Node] [parent : (Option FPTree)] [children : (Listof FPTree)]) #:mutable #:transparent)

(struct: FP-Tree ([root : FPTree][headers : HeaderList]) #:transparent)
