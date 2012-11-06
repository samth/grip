#lang typed/racket/base

(provide:
 [load-tab-delimited-file (Path [#:schema (Option Schema)] -> Frame)])

(require
 racket/match
 (only-in "schema.rkt"
          generate-anon-series-names
          Schema SeriesTypes Schema-has-headers 
          Schema-SeriesTypes Schema-headers)
 "frame-builder.rkt"
 (only-in prelude/type/list
          zip)
 (only-in "series-builder.rkt"
          SeriesBuilderTypes)
 (only-in "numeric-series-builder.rkt"
          mkNumericSeriesBuilder
          NumericSeriesBuilder 
          NumericSeriesBuilder?
          complete-NumericSeriesBuilder)
 (only-in "categorical-series-builder.rkt"
          mkCategoricalSeriesBuilder
          CategoricalSeriesBuilder
          CategoricalSeriesBuilder?
          complete-CategoricalSeriesBuilder
          append-CategoricalSeriesBuilder)
 (only-in "tab-delimited.rkt"
          read-tab-delimited-file
          sample-tab-delimited-file)
 (only-in "../frame/frame.rkt"
          Frame FrameSeries
          mkFrame))

(: mkFrameBuilder-from-Schema (Schema -> FrameBuilder))
(define (mkFrameBuilder-from-Schema schema)
  
  (: determine-SeriesBuilder (SeriesTypes -> SeriesBuilderTypes))
  (define (determine-SeriesBuilder stypes)    
    (match stypes
      ['CATEGORICAL (mkCategoricalSeriesBuilder)]
      ['NUMERIC     (mkNumericSeriesBuilder)]))
  
  (FrameBuilder ((inst map SeriesBuilderTypes SeriesTypes) 
                 determine-SeriesBuilder 
                 (Schema-SeriesTypes schema))))

(: complete-SeriesBuilders (FrameBuilder -> (Listof FrameSeries)))
(define (complete-SeriesBuilders frame-builder)
  (map (λ: ((builder : SeriesBuilderTypes))
         (cond
           [(CategoricalSeriesBuilder? builder)
            (complete-CategoricalSeriesBuilder builder)]
           [(NumericSeriesBuilder? builder)
            (complete-NumericSeriesBuilder builder)]
           [else (error "Inconsistent FrameBuilder")]))
       (FrameBuilder-builders frame-builder)))

(: anon-headers (Integer -> (Listof Symbol)))
(define (anon-headers cnt)
  (map string->symbol (generate-anon-series-names cnt)))

(: make-frame (Schema FrameBuilder -> Frame))
(define (make-frame schema builder)
  (let ((cols (complete-SeriesBuilders builder)))
    (let ((headers (if (Schema-has-headers schema) 
                       (Schema-headers schema) 
                       (anon-headers (length cols)))))
      (mkFrame ((inst zip Symbol FrameSeries) headers cols)))))

(: load-tab-delimited-file (Path [#:schema (Option Schema)] -> Frame))
(define (load-tab-delimited-file fpath #:schema [schema #f])
  (define SAMPLE-SIZE 20)
  (let ((schema (if schema schema (sample-tab-delimited-file fpath SAMPLE-SIZE))))
    (let ((builder (read-tab-delimited-file fpath 
                                            (Schema-has-headers schema) 
                                            (mkFrameBuilder-from-Schema schema))))
      (make-frame schema builder))))