#lang typed/racket/base

(provide:
 [series-complete (SeriesBuilder -> Series)])

(require
 (only-in "series-description.rkt"
	  Series)
 (only-in "series-builder.rkt"
	  SeriesBuilder)
 (only-in "categorical-series-builder.rkt"
	  CSeriesBuilder CSeriesBuilder?
	  complete-CSeriesBuilder)
 (only-in "numeric-series-builder.rkt"
	  NSeriesBuilder NSeriesBuilder?
	  complete-NSeriesBuilder)
 (only-in "integer-series-builder.rkt"
	  ISeriesBuilder ISeriesBuilder?
	  complete-ISeriesBuilder))


(: series-complete (SeriesBuilder -> Series))
(define (series-complete builder)
  (cond
   ((NSeriesBuilder? builder)
    (complete-NSeriesBuilder builder))
   ((CSeriesBuilder? builder)
    (complete-CSeriesBuilder builder))
   ((ISeriesBuilder? builder)
    (complete-ISeriesBuilder builder))))


