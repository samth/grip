#lang typed/racket/base

(provide 
 rdd-print
 rdd-map
 rdd-filter)

(require
 racket/match
 (only-in "rdd.rkt"	
          rdd-text
          RDDPrint
          RDDFilter
          RDDMap
	  RDD))

;; (: rdd->sequence (All (T) (RDD T) -> (Sequenceof T)))
;; (define (rdd->sequence rdd)
;;   (match rdd
;;     ((RDDList block)
;;      (in-list block))))    

(: rdd-map  (All (A B) RDD (A -> B) -> RDD))
(define (rdd-map rdd f) 
    (RDDMap rdd f))

(: rdd-filter (All (T) RDD (T -> Boolean) -> RDD))
(define (rdd-filter rdd f)
  (RDDFilter rdd f))

(: rdd-print (RDD -> RDD))
(define (rdd-print rdd)
  (RDDPrint rdd))

