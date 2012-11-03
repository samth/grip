#lang typed/racket/base

(require
 (only-in iteratee/iteratee
          Enumeratee)
 (only-in iteratee/iterfile
          IOResult)
 (only-in iteratee/enumeratees
          enumeratee-transform)
 (only-in "rdd.rkt" 
          RDD RDDMap RDDMap-cvt))

(define-type RDDResult IOResult)

;;(: rdd-compile (All (T) (RDD T) -> (Enumeratee  IOStatus)))

(: compile-map (All (O I) (RDDMap O I) -> (Enumeratee O I RDDResult)))
(define (compile-map rdd-map)
  (define: cvt : (O -> I) (RDDMap-cvt rdd-map))
  (enumeratee-transform cvt))