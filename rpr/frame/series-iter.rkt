#lang typed/racket/base

(provide
 CSeriesFn)

(provide:
 [cseries-map (CSeries CSeriesFn -> CSeries)]
 [cseries-lift-fn ((Label -> Label) -> (CSeries -> CSeries))])

(require
 (only-in "indexed-series.rkt"
	  Label)
 (only-in "categorical-series.rkt"
	  CSeries
	  cseries-referencer cseries-count)
 (only-in "categorical-series-builder.rkt"
	  new-CSeriesBuilder append-CSeriesBuilder complete-CSeriesBuilder))

(define-type CSeriesFn (Label -> Label))

(: cseries-lift-fn ((Label -> Label) -> (CSeries -> CSeries)))
(define (cseries-lift-fn l-fn)
  (λ: ((cs : CSeries))
      (cseries-map cs l-fn)))
      
(: cseries-map (CSeries CSeriesFn -> CSeries))
(define (cseries-map cseries cfn)
  (let* ((crefer (cseries-referencer cseries))
	 (len    (cseries-count cseries))
	 (builder (new-CSeriesBuilder len)))
    (do ([i 0 (add1 i)])
	([>= i len] (complete-CSeriesBuilder builder))
      (append-CSeriesBuilder builder (cfn (crefer (assert i index?)))))))	      
