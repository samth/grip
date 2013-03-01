#lang typed/racket/base

(provide:
 [series-complete (SeriesBuilder -> Series)])

(require
 (only-in "series-description.rkt"
	  Series)
 (only-in "../load/series-builder.rkt"
	  SeriesBuilder)
 (only-in "../load/categorical-series-builder.rkt"
	  CSeriesBuilder CSeriesBuilder?
	  complete-CSeriesBuilder)
 (only-in "../load/numeric-series-builder.rkt"
	  NSeriesBuilder NSeriesBuilder?
	  complete-NSeriesBuilder)
 (only-in "../load/integer-series-builder.rkt"
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

