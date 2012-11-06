#lang typed/racket/base

(provide
 SeriesBuilderTypes)

(require
 (only-in "numeric-series-builder.rkt"
          NumericSeriesBuilder)
 (only-in "categorical-series-builder.rkt"
          CategoricalSeriesBuilder))

(define-type SeriesBuilderTypes (U CategoricalSeriesBuilder NumericSeriesBuilder))

