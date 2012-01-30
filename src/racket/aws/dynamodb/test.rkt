#lang typed/racket/base

(require
 (only-in "createtable.rkt"
	  create-table Key Throughput))

(define (create)
  (create-table "person" (Key "id" 'String) #f (Throughput 1 1)))

(define (add)
  (void))
