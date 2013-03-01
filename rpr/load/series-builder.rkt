#lang typed/racket/base

(provide
 SeriesBuilder)

(require
 (only-in "numeric-series-builder.rkt"
          NSeriesBuilder)
 (only-in "categorical-series-builder.rkt"
          CSeriesBuilder)
 (only-in "integer-series-builder.rkt"
	  ISeriesBuilder))

(define-type SeriesBuilder (U ISeriesBuilder CSeriesBuilder NSeriesBuilder))

