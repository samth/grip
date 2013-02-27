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
          SeriesBuilder)
 (only-in "numeric-series-builder.rkt"
          new-NSeriesBuilder
          NSeriesBuilder 
          NSeriesBuilder?
          complete-NSeriesBuilder)
 (only-in "categorical-series-builder.rkt"
          new-CSeriesBuilder
          CSeriesBuilder
          CSeriesBuilder?
          complete-CSeriesBuilder
          append-CSeriesBuilder)
 (only-in "tab-delimited.rkt"
          read-tab-delimited-file
          sample-tab-delimited-file)
 (only-in "../frame/series-description.rkt"
          Series)
 (only-in "../frame/frame.rkt"
          Frame 
          new-frame))

(: new-FrameBuilder-from-Schema (Schema -> FrameBuilder))
(define (new-FrameBuilder-from-Schema schema)
  
  (: determine-SeriesBuilder (SeriesTypes -> SeriesBuilder))
  (define (determine-SeriesBuilder stypes)    
    (match stypes
	   ['CATEGORICAL (new-CSeriesBuilder)]
	   ['NUMERIC     (new-NSeriesBuilder)]))
  
  (FrameBuilder ((inst map SeriesBuilder SeriesTypes) 
                 determine-SeriesBuilder 
                 (Schema-SeriesTypes schema))))

(: complete-SeriesBuilders (FrameBuilder -> (Listof Series)))
(define (complete-SeriesBuilders frame-builder)
  (map (λ: ((builder : SeriesBuilder))
	   (cond
	    [(CSeriesBuilder? builder)
	     (complete-CSeriesBuilder builder)]
	    [(NSeriesBuilder? builder)
	     (complete-NSeriesBuilder builder)]
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
      (new-frame ((inst zip Symbol Series) headers cols)))))

(: load-tab-delimited-file (Path [#:schema (Option Schema)] -> Frame))
(define (load-tab-delimited-file fpath #:schema [schema #f])
  (define SAMPLE-SIZE 20)
  (let ((schema (if schema schema (sample-tab-delimited-file fpath SAMPLE-SIZE))))
    (let ((builder (read-tab-delimited-file fpath 
                                            (Schema-has-headers schema) 
                                            (new-FrameBuilder-from-Schema schema))))
      (make-frame schema builder))))
