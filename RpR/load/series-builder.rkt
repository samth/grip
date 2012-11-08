#lang typed/racket/base

(provide
 SeriesBuilderTypes)

(require
 (only-in "numeric-series-builder.rkt"
          NSeriesBuilder)
 (only-in "categorical-series-builder.rkt"
          CSeriesBuilder))

(define-type SeriesBuilderTypes (U CSeriesBuilder NSeriesBuilder))

