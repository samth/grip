#lang typed/racket/base

(provide
 (struct-out CrossTabulation))

(require
 (only-in "../frame/series.rkt"
          SIndex)
 (only-in "../frame/categorical-series.rkt"
          CategoricalSeries
          CategoricalSeries->SIndex
          CategoricalSeries-data
          CategoricalSeries-nominals))

;; A CategoricalSeries is internally a vector of integer values.
;; Each integer value denotes a nominal values.
;; e.g Gender = {Male=1, Female=2} 
;; The stride is the cardinality of the first nominal.
;; Constraint |nom1| < stride

(struct: CrossTabulation ([nominal1 : (Vectorof Symbol)] ;; nominals
                          [nominal2 : (Vectorof Symbol)]
                          [freqs : (Vectorof Integer)]) #:transparent)

(: crosstabulate (CategoricalSeries CategoricalSeries -> CrossTabulation))
(define (crosstabulate cs1 cs2)  
  (define d1 (CategoricalSeries-data cs1)) 
  (define d2 (CategoricalSeries-data cs2))    
  (define stride (vector-length (CategoricalSeries-nominals cs1))) 
  (define len (vector-length d1))
  
  (: consistent-series (-> Boolean))
  (define (consistent-series)    
    (eq? len (vector-length d2)))  
  
  (unless (consistent-series)
    (error "Cannot cross tabulate two CategoricalSeries with differing data lengths."))
    
  (define xtab-counts (make-vector (* (vector-length (CategoricalSeries-nominals cs1))
                                      (vector-length (CategoricalSeries-nominals cs2))) 0))
  
  (do ([idx 0 (add1 idx)])
    ([>= idx len] (CrossTabulation (CategoricalSeries-nominals cs1)
                                   (CategoricalSeries-nominals cs2)
                                   xtab-counts))
    (let ((j (+ (vector-ref d1 idx) (* (vector-ref d2 idx) stride))))
      (vector-set! xtab-counts j (add1 (vector-ref xtab-counts j))))))
        
(: test (-> CrossTabulation))
(define (test)
  (define cs1 
    (CategoricalSeries #f
                       '#(1 0 1 0 0 0 1 1 0 1)
                       '#[Male Female]))
  (define cs2
    (CategoricalSeries #f
                       '#(0 1 0 0 1 0 0 1 0 0)
                       '#(Right Left)))
  (crosstabulate cs1 cs2))
    
    