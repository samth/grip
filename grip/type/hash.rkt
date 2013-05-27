#lang typed/racket/base

(provide:
 [project (All (K V) (HashTable K V) (Listof K) -> (Listof V))])

(: project (All (K V) (HashTable K V) (Listof K) -> (Listof V)))
(define (project ht vs)
  (for/list [(k (in-list vs))]
	    (hash-ref ht k)))
