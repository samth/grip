#lang typed/racket/base

(provide
 compile-print
 compile-list
 compile-filter
 compile-map
 RDDResult)

(require
 (only-in "../../io/iteratee/iteratee.rkt"
          Enumerator
          Enumeratee)
 (only-in "../../io/iteratee/iterfile.rkt"
          IOResult)
 (only-in "../../io/iteratee/enumeratees.rkt"
          enumeratee-filter
          enumeratee-transform)
 (only-in "../../io/iteratee/enumerators.rkt"
          enumerator/list)
 (only-in "../../io/iteratee/iterfile.rkt"
          TextPortIteratee
          iter-text-port)
 (only-in "rdd.rkt" 
          RDD 
          RDDPrint
          RDDList RDDList-xs
          RDDMap RDDMap-cvt
          RDDFilter RDDFilter-filterfn))

(define-type RDDResult IOResult)

(: compile-list (All (A B) (RDDList A B) -> (Enumerator A B)))
(define (compile-list rdd-lst)
  (enumerator/list (RDDList-xs rdd-lst)))

(: compile-map (All (O I) RDDMap -> (Enumeratee O I RDDResult)))
(define (compile-map rdd-map)
  (define: cvt : (O -> I) (RDDMap-cvt rdd-map))
  (enumeratee-transform cvt))

(: compile-filter (All (T) RDDFilter -> (Enumeratee T T RDDResult)))
(define (compile-filter rdd-filter)
  (define: f : (T -> Boolean) (RDDFilter-filterfn rdd-filter))
  (enumeratee-filter (RDDFilter-filterfn rdd-filter)))

(: compile-print (RDDPrint -> TextPortIteratee))
(define (compile-print rdd-print)
  (iter-text-port (current-output-port)))