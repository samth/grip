#lang typed/racket/base

(provide
 (struct-out SeriesDescription)
 Series)

(provide:
 [frame-series-type-label (Series -> Symbol)] 
 [series-count (Series -> Nonnegative-Integer)])

(require 
 (only-in racket/flonum
          flvector-length)
 (only-in "series.rkt"
          Label
          GSeries GSeries? GSeries-data gseries-count)         
 (only-in "categorical-series.rkt"
          CSeries CSeries? CSeries-data cseries-count)
 (only-in "numeric-series.rkt"
          NSeries NSeries? NSeries-data nseries-count))
          
(define-type Series (U GSeries NSeries CSeries))

(struct: SeriesDescription ([name : Label]
                            [type : Label]
                            [count : Integer]))

(: frame-series-type-label (Series -> Symbol))
(define (frame-series-type-label series)
  (cond
    ((GSeries? series) 'GenericSeries)
    ((NSeries? series) 'NumericSeries)
    ((CSeries? series) 'CategoricalSeries)
    (else 'UnknownSeries)))

(: series-count (Series -> Nonnegative-Integer))
(define (series-count series)
  (cond
    [(NSeries? series) (nseries-count series)]     
    [(CSeries? series) (cseries-count series)]     
    [(GSeries? series) (gseries-count series)]     
    [else (error "Unknown Series type in Frame")]))
    
  
  