#lang typed/racket/base

;(provide
; CategoricalSeriesBuilder 
; mkCategoricalSeriesBuilder 
; append-SeriesBuilder
; complete-SeriesBuilder)

(require 
 (only-in "categorical-series.rkt"
          CSeries))

;; (struct: (D S) SeriesBuilder ([append : (D -> Void)]
;;                               [complete : (-> S)]))

;; (: append-SeriesBuilder (All (D S) (SeriesBuilder D S) D -> Void))
;; (define (append-SeriesBuilder builder datum)
;;   ;; BUG TYPED RACKET???
;;   ;;((SeriesBuilder-append builder) datum))
;;   (void))

;; (: complete-SeriesBuilder (All (D S) (SeriesBuilder D S) -> S))
;; (define (complete-SeriesBuilder builder)
;;   ((SeriesBuilder-complete builder)))
