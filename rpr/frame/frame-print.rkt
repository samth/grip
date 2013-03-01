#lang typed/racket/base

(provide:
 [frame-head (case-> (Frame -> Void)
		     (Frame (Option Index) -> Void))])

(require
 (only-in prelude/type/format
	  ~a ~r)
 (only-in "indexed-series.rkt"
	  Label)
 (only-in "series-description.rkt"
	  Series series-type)
 (only-in "frame.rkt"
	  Frame new-frame frame-names
	  Column Columns column-heading column-series
	  frame-cseries frame-explode
	  FrameDescription FrameDescription-series 
	  show-frame-description frame-description)
 (only-in "integer-series.rkt"
	  ISeries ISeries? iseries-ref)
 (only-in "numeric-series.rkt"
	  NSeries NSeries? nseries-ref)
 (only-in "categorical-series.rkt"
	  cseries-referencer cseries-count cseries-ref
	  CSeries CSeries?))

(define WIDTH 15)

(define-type FrameRowFormatter (Frame Natural -> String))

(: display-heading (Columns -> Void))
(define (display-heading cols)

  (: format-heading (Symbol -> String))
  (define (format-heading heading)
    (~a (symbol->string heading)
	#:width WIDTH
	#:align 'center))
 
  (for ([col cols])
       (let ((heading (column-heading col))
	     (series  (column-series col)))
	 (cond 
	  ((NSeries? series)
	   (display (format-heading heading)))
	  ((CSeries? series)
	   (display (format-heading heading)))
	  ((ISeries? series)
	   (display (format-heading heading)))
	  (else
	   (error 'frame-head "Heading for unknown series types ~s" 
		  (series-type series)))))
       (display " "))

  (newline))

(: format-cseries (CSeries Index -> String))
(define (format-cseries cseries row)
  (~a (symbol->string (cseries-ref cseries row))
      #:width WIDTH
      #:align 'left))

(: format-nseries (NSeries Index -> String))
(define (format-nseries nseries row)
  (~r (nseries-ref nseries row)
      #:precision '(= 4)
      #:min-width WIDTH))

(: format-iseries (ISeries Index -> String))
(define (format-iseries iseries row)
  (~r (iseries-ref iseries row)
      #:precision 0
      #:min-width WIDTH))

(: display-frame-row ((Vectorof Series) (Sequenceof Index) -> Void))
(define (display-frame-row series rows)
  ;;  (define: cols : (Sequenceof  (in-range (vector-length series)))
  (for: ([row : Index rows])
	(for ([col (in-range (vector-length series))])
	     (let ((a-series (vector-ref series col)))
	       (cond
		((NSeries? a-series)
		 (display (format-nseries a-series row)))
		((CSeries? a-series)
		 (display (format-cseries a-series row)))
		((ISeries? a-series)
		 (display (format-iseries a-series row)))
		(else
		 (error 'frame-head "Unknown series types ~s" 
			(series-type a-series))))))
	(display " ")
	(newline)))

(define default-head-rows 10)

(: frame-head (case-> (Frame -> Void)
		      (Frame (Option Index) -> Void)))
(define (frame-head frame [count #f])
  (define: cols     : Columns (frame-explode frame))
  (define: headings : (Listof Label) (map column-heading cols))
  (define: series   : (Vectorof Series) (list->vector (map column-series cols)))
  
  ;; (show-frame-description (frame-description frame))

  (display-heading cols)
  (let ((count (if (not count) default-head-rows count)))
    (display-frame-row series (in-range count))))
