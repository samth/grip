#lang typed/racket/base

(provide
 sample-tab-delimited-file
 read-tab-delimited-file)

(require "../frame/frame.rkt"
         racket/match
         racket/pretty
         (only-in "../../io/iteratee/enumerators.rkt"
                  enumerator/text-input-port)
         (only-in "../../io/iteratee/iteratee.rkt"
                  Iteratee Stream Continue Done
                  icomplete)
         (only-in "../../io/iteratee/iteratees.rkt"
                  head-n)
         (only-in "parse.rkt"
                  parse-tab-line)
         (only-in "schema.rkt"
                  generate-anon-series-names
                  determine-Schema
                  Schema)
         (only-in "categorical-series-builder.rkt"
                  CSeriesBuilder
                  CSeriesBuilder?
                  complete-CSeriesBuilder
                  append-CSeriesBuilder)
         (only-in "numeric-series-builder.rkt"
                  NSeriesBuilder
                  NSeriesBuilder?
                  append-NSeriesBuilder)
         (only-in "series-builder.rkt"
                  SeriesBuilderTypes)                  
         (only-in "frame-builder.rkt"
                  append-data-fields
                  FrameBuilder
                  FrameBuilder-builders))

(: tab-record-iteratee (FrameBuilder -> (Iteratee String FrameBuilder)))
(define (tab-record-iteratee frame-builder)
  
  (: appenders (Listof (String -> Void)))
  (define appenders (map (λ: ((builder : SeriesBuilderTypes))
                           (cond
                             [(CSeriesBuilder? builder)
                              (λ: ((str : String))
                                (append-CSeriesBuilder builder str))]
                             [(NSeriesBuilder? builder)
                              (λ: ((str : String))
                                (append-NSeriesBuilder builder str))]
                             [else (λ: ((str : String)) (void))]))
                         (FrameBuilder-builders frame-builder)))
  
  (: step ((Stream String) -> (Iteratee String FrameBuilder)))
  (define (step input)            
    (match input
      ['Nothing (Continue step)]
      ['EOS     (Done 'EOS frame-builder)] ;; (complete-FrameBuilder frame-builder))]
      [str      (begin
                  (when (string? str)                    
                    (append-data-fields appenders (parse-tab-line str))
                    (void))
                  (Continue step))]))
  
  (Continue step))

(: check-data-file-exists (Path -> Void))
(define (check-data-file-exists fpath)
  (unless (file-exists? fpath)
    (error (format "File not found: ~s" (path->string fpath)))))

(: read-tab-delimited-file (Path Boolean FrameBuilder -> FrameBuilder))
(define (read-tab-delimited-file fpath headers builder)
  (check-data-file-exists fpath)
  (call-with-input-file* 
      fpath
    (λ: ((inp : Input-Port))   
      (when headers (read-line inp))
      (icomplete (((inst enumerator/text-input-port FrameBuilder) inp)
                  (tab-record-iteratee builder))))))

(: sample-tab-delimited-file (Path Integer -> Schema))
(define (sample-tab-delimited-file fpath cnt)
  (check-data-file-exists fpath)
  (call-with-input-file* 
      fpath
    (λ: ((inp : Input-Port))                   
      (determine-Schema (icomplete (((inst enumerator/text-input-port (Listof String)) inp)
                                    ((inst head-n String) cnt)))))))

