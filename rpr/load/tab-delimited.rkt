#lang typed/racket/base

(provide
 sample-tab-delimited-file
 read-tab-delimited-file)

(require
 "../frame/frame.rkt"
 racket/match
 racket/pretty
 (only-in system/filepath
	  FilePath FilePath->string)
 (only-in iteratee
	  enumerator/text-input-port
	  Iteratee Stream Continue Done
	  icomplete
	  head-n)
 (only-in "../frame/categorical-series-builder.rkt"
	  CSeriesBuilder
	  CSeriesBuilder?
	  complete-CSeriesBuilder
	  append-CSeriesBuilder)
 (only-in "../frame/integer-series-builder.rkt"
	  ISeriesBuilder ISeriesBuilder?
	  complete-ISeriesBuilder
	  append-ISeriesBuilder)
 (only-in "../frame/numeric-series-builder.rkt"
	  NSeriesBuilder
	  NSeriesBuilder?
	  append-NSeriesBuilder)
 (only-in "../frame/series-builder.rkt"
	  SeriesBuilder)
 (only-in "frame-builder.rkt"
	  append-data-fields
	  FrameBuilder
	  FrameBuilder-builders)
 (only-in "parse.rkt"
	  parse-tab-line)
 (only-in "schema.rkt"
	  generate-anon-series-names
	  determine-Schema
	  Schema))

(: tab-record-iteratee (FrameBuilder -> (Iteratee String FrameBuilder)))
(define (tab-record-iteratee frame-builder)

  (: appenders (Listof (String -> Void)))
  (define appenders (map (λ: ((builder : SeriesBuilder))
			     (cond
			      [(CSeriesBuilder? builder)
			       (λ: ((str : String))
				   (append-CSeriesBuilder builder str))]
			      [(ISeriesBuilder? builder)
			       (λ: ((str : String))
				   (append-ISeriesBuilder builder str))]
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

(: check-data-file-exists (FilePath -> Void))
(define (check-data-file-exists fpath)
  (unless (file-exists? (FilePath->string fpath))
	  (error (format "File not found: ~s" (FilePath->string fpath)))))

(: read-tab-delimited-file (FilePath Boolean FrameBuilder -> FrameBuilder))
(define (read-tab-delimited-file fpath headers builder)
  (check-data-file-exists fpath)
  (let ((fpath (FilePath->string fpath)))
    (call-with-input-file*
     fpath
     (λ: ((inp : Input-Port))
	 (when headers (read-line inp))
	 (icomplete (((inst enumerator/text-input-port FrameBuilder) inp)
		     (tab-record-iteratee builder)))))))

(: sample-tab-delimited-file (FilePath Integer -> Schema))
(define (sample-tab-delimited-file fpath cnt)
  (check-data-file-exists fpath)
  (let ((fpath (FilePath->string fpath)))
    (call-with-input-file*
     fpath
     (λ: ((inp : Input-Port))
	 (determine-Schema (icomplete (((inst enumerator/text-input-port (Listof String)) inp)
				       ((inst head-n String) cnt))))))))
