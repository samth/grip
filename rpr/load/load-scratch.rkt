#lang typed/racket/base

(provide main)

(require
 (only-in "../frame/series-builder.rkt"
          SeriesBuilder)
 (only-in "../frame/categorical-series-builder.rkt"
          new-CSeriesBuilder)
 (only-in "schema.rkt"
          SeriesTypes
          alter-schema-no-headers)
 (only-in "tab-delimited.rkt"
          sample-tab-delimited-file
          read-tab-delimited-file)
 (only-in "frame-builder.rkt"
          FrameBuilder)
 (only-in "load.rkt"
          load-tab-delimited-file))

(define infile (string->path "/code/custanalysis/ad100.txt"))

(: test-builder FrameBuilder)
(define test-builder
  (FrameBuilder (do: : (Listof SeriesBuilder)
		     ((i : Integer 0 (add1 i))
		      (accum : (Listof SeriesBuilder) '()
			     (cons (new-CSeriesBuilder)
				   accum)))
		     ((>= i 3) accum))))

(define (test)
  (read-tab-delimited-file infile #f test-builder))

(define (test1)
  (sample-tab-delimited-file infile 10))

(define (test2)
  (alter-schema-no-headers (sample-tab-delimited-file infile 10)))

(define (test3)
  (load-tab-delimited-file infile))

(define (main)
  (define infile (string->path "/code/custanalysis/ad100.txt"))
  (let ((f (load-tab-delimited-file infile)))
    f))

