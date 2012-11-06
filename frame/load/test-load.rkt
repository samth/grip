#lang typed/racket/base

(provide main)

(require
 (only-in "schema.rkt"
          SeriesTypes
          alter-schema-no-headers)
 (only-in "../frame/frame.rkt"
          Frame-names
          Frame-dim)
 (only-in "tab-delimited.rkt"
          sample-tab-delimited-file
          read-tab-delimited-file)
 (only-in "series-builder.rkt"
          SeriesBuilderTypes)
 (only-in "categorical-series-builder.rkt"
          mkCategoricalSeriesBuilder)
 (only-in "frame-builder.rkt"
          FrameBuilder)
 (only-in "load.rkt"
          load-tab-delimited-file))

(define infile (string->path "/code/custanalysis/ad100.txt"))

(: test-builder FrameBuilder)
(define test-builder
  (FrameBuilder (do: : (Listof SeriesBuilderTypes)
                  ((i : Integer 0 (add1 i))
                   (accum : (Listof SeriesBuilderTypes) '()
                          (cons (mkCategoricalSeriesBuilder)
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

