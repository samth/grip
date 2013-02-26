#lang typed/racket/base

(provide
 SeriesBuilder)

(require
 (only-in "numeric-series-builder.rkt"
          NSeriesBuilder)
 (only-in "categorical-series-builder.rkt"
          CSeriesBuilder))

(define-type SeriesBuilder (U CSeriesBuilder NSeriesBuilder))

