#lang typed/racket/base

(provide 
 rdd-map)

(require
 racket/match
 (only-in "rdd.rkt"
          RDDList RDD))

(: rdd->sequence (All (T) (RDD T) -> (Sequenceof T)))
(define (rdd->sequence rdd)
  (match rdd
    ((RDDList block)
     (in-list block))))    

(: rdd-map (All (T V) (RDD T) (T -> V) -> (RDD V)))
(define (rdd-map rdd f) 
  (match rdd
    ((RDDList block)
     (RDDList (map f block)))))

;(: rdd-filter (All (T) (RDD T) (T -> Boolean) -> (RDD T)))
;(define (rdd-filter rdd f)
;  (match rdd 
;    [(RDDList block)
;     (RDDList (filter f block))]
;    [(RDDFile blocks)
;     (RDDList (filter f block
     
    

