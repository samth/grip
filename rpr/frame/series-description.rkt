#lang typed/racket/base

(provide
 (struct-out SeriesDescription)
 Series SeriesType)

(provide:
 [series-type (Series -> SeriesType)]
 [series-count (Series -> Index)])

(require 
 (only-in racket/flonum
          flvector-length)
 (only-in "indexed-series.rkt"
          Label
          GSeries GSeries? GSeries-data gseries-count)         
 (only-in "categorical-series.rkt"
          CSeries CSeries? CSeries-data cseries-count)
 (only-in "numeric-series.rkt"
          NSeries NSeries? NSeries-data nseries-count)
 (only-in "integer-series.rkt"
	  ISeries ISeries? ISeries-data iseries-count))

(define-type Series (U GSeries NSeries CSeries ISeries))

(define-type SeriesType (U 'NumericSeries 'CategoricalSeries 'IntegerSeries))

(struct: SeriesDescription ([name : Label]
                            [type : SeriesType]
                            [count : Integer]) #:transparent)

(: series-type (Series -> SeriesType))
(define (series-type series)
  (cond
   ;; ((GSeries? series) 'GenericSeries)
   ((NSeries? series) 'NumericSeries)
   ((CSeries? series) 'CategoricalSeries)
   ((ISeries? series) 'IntegerSeries)
   (else (error 'frame-series-description-type "'UnknownSeries: ~a" series))))

(: series-count (Series -> Index))
(define (series-count series)
  (cond
   [(NSeries? series) (nseries-count series)]     
   [(CSeries? series) (cseries-count series)]     
   ;;    [(GSeries? series) (gseries-count series)]     
   [(ISeries? series) (iseries-count series)]
   [else (error "Unknown Series type in Frame")]))
      
