#lang typed/racket/base

(provide:
 [column-heading (Column -> Label)]
 [column-series (Column -> Series)]
 [frame-rename (Frame Label Label -> Frame)]
 [frame-drop (Frame Label -> Frame)]
 [frame-explode (Frame [#:project LabelProjection] -> Columns)]
 [frame-append  (Frame (U Column Columns Frame) -> Frame)]
 [frame-description (Frame [#:project LabelProjection] -> FrameDescription)]
 [show-frame-description (FrameDescription -> Void)])

(provide
 Frame Column Columns
 (struct-out FrameDescription)
 frame-series
 frame-names frame-dim 
 frame-cseries frame-nseries
 frame-series
 new-frame)

(require 
 (only-in racket/vector
	  vector-copy)
 (only-in racket/set
	  set-empty? set-member?
	  list->set)
 (only-in "types.rkt"
          Dim Dim-rows Dim-cols)
 (only-in "indexed-series.rkt"
	  label-sort-positional
          Label LabelProjection LabelIndex LabelIndex-index
          GSeries SIndex
          build-index-from-labels label-index)
 (only-in "series-description.rkt"
          series-count
          series-type
          Series 
          SeriesDescription SeriesDescription-name
          SeriesDescription-type SeriesDescription-count)
 (only-in "categorical-series.rkt"
          CSeries CSeries?
          CSeries-data)
 (only-in "numeric-series.rkt"
          NSeries NSeries? 
          NSeries-data
          mkNSeries)
 (only-in "integer-series.rkt"
	  ISeries ISeries?
	  ISeries-data
	  new-ISeries))

(define-type Column  (Pair Label Series))
(define-type Columns (Listof Column))

;; A frame is map of series.
(struct: Frame LabelIndex ([series : (Vectorof Series)]) #:transparent)

(struct: FrameDescription ([dimensions : Dim]
                           [series : (Listof SeriesDescription)]) #:transparent)


(: column-heading (Column -> Label))
(define (column-heading col)
  (car col))

(: column-series (Column -> Series))
(define (column-series col)
  (cdr col))

(: new-frame (Columns -> Frame))
(define (new-frame cols)
  
  (define (check-equal-length)
    (when  (pair? cols)
	   (let ((len (if (null? cols) 
			  0 
			  (series-count (cdr (car cols))))))
	     (unless (andmap (λ: ((s : (Pair Symbol Series)))
				 (eq? len (series-count (cdr s))))
			     (cdr cols))
		     (error 'new-frame "Frame must have equal length series.")))))
  
  (check-equal-length)
  (let ((index (build-index-from-labels ((inst map Label Column)
                                         (inst car Label Series) cols)))
        (data (apply vector ((inst map Series Column) cdr cols))))
    (Frame index data)))

(: frame-rename (Frame Label Label -> Frame))
(define (frame-rename frame from to)
  (let ((index (LabelIndex-index frame)))
    (if index
	(let: ((col-idx : (Option Index) (hash-ref index from (λ () #f))))
	      (if col-idx
		  (let ((new-index (hash-copy index)))
		    (hash-remove! new-index from)
		    (hash-set! new-index to col-idx)
		    (Frame new-index (vector-copy (Frame-series frame))))
		  frame))
	frame)))

(: frame-drop (Frame Label -> Frame))
(define (frame-drop frame label)
  (new-frame (filter (λ: ((col : Column))
			 (not (eq? (car col) label)))
		     (frame-explode frame))))

(: frame-series (Frame Symbol -> Series))
(define (frame-series frame col)
  (vector-ref (Frame-series frame)
              (label-index (assert (LabelIndex-index frame)) col)))

(: frame-cseries (Frame Symbol -> CSeries))
(define (frame-cseries frame name)
  (assert (frame-series frame name) CSeries?))

(: frame-nseries (Frame Symbol -> NSeries))
(define (frame-nseries frame name)
  (assert (frame-series frame name) NSeries?))

(: Frame-iseries (Frame Symbol -> ISeries))
(define (Frame-iseries frame name)
  (assert (frame-series frame name) ISeries?))

(: frame-labels (Frame -> (Listof (Pair Symbol Index))))
(define (frame-labels frame)
  (hash->list (assert (LabelIndex-index frame))))

(: frame-names (Frame -> (Listof Symbol)))
(define (frame-names frame)  
  (map (λ: ((kv : (Pair Symbol Integer)))
	   (car kv))
       ((inst sort (Pair Symbol Index) (Pair Symbol Index))
        (frame-labels frame)
        (λ: ((kv1 : (Pair Symbol Index)) 
             (kv2 : (Pair Symbol Index)))
	    (< (cdr kv1) (cdr kv2))))))

(: frame-dim (Frame -> Dim))
(define (frame-dim frame)
  (let ((cols (length (hash-keys (assert (LabelIndex-index frame))))))
    (if (zero? cols)
        (Dim 0 0)
        (let ((rows (let ((series (vector-ref (Frame-series frame) 0)))
                      (series-count series))))                      
          (Dim rows cols)))))

(: projection-filter (All (A) (Listof A) (A -> Symbol) LabelProjection -> (Listof A)))
(define (projection-filter lst sym-fn project)
  
  (: projection-set (LabelProjection -> (Setof Label)))
  (define (projection-set labels)
    (if (list? project) 
	(list->set project) 
	project))
  
  (define projection (projection-set project))
  
  (if (set-empty? projection)
      lst
      (filter (λ: ((a : A))
		  (set-member? projection (sym-fn a)))
	      lst)))

(: frame-description (Frame [#:project LabelProjection] -> FrameDescription))
(define (frame-description frame #:project [project '()])
  
  (let ((names (frame-names frame)))
    (let: loop : FrameDescription ((names : (Listof Label) names) 
				   (descs : (Listof SeriesDescription) '()))
	  (if (null? names)
	      (let ((dim (frame-dim frame))
		    (cols (projection-filter descs 
					     (λ: ((sd : SeriesDescription))
						 (SeriesDescription-name sd))
					     project)))
		(FrameDescription (Dim (Dim-rows dim)
				       (length cols))
				  (reverse cols)))
	      (let* ((name (car names))
		     (series (frame-series frame name)))
		(loop (cdr names) (cons (SeriesDescription name 
							   (series-type series) 
							   (series-count series))
					descs)))))))

;; Really need to enumerate a minimal set of generic functions, such as `show'
(: show-frame-description (FrameDescription -> Void))
(define (show-frame-description fdesc)
  
  (: print-series-description (SeriesDescription -> Void))
  (define (print-series-description sdesc)
    (displayln (format "  - ~a: ~a"
		       (SeriesDescription-name sdesc)
		       (SeriesDescription-type sdesc))))                      
  
  (let ((dim (FrameDescription-dimensions fdesc)))
    (displayln (format "Frame::(Cols: ~a, Rows: ~a)" (Dim-cols dim) (Dim-rows dim)))
    (for-each print-series-description (FrameDescription-series fdesc))))

(: frame-explode (Frame [#:project LabelProjection] -> Columns))
(define (frame-explode frame #:project [project '()])
  
  (let ((labeling (label-sort-positional frame))
	(series (Frame-series frame)))
    (projection-filter (for/list: : Columns
				  ([label labeling])
				  (cons (car label)
					(vector-ref series (cdr label))))
		       (λ: ((l-s : Column)) ;; need to assist TR here.
			   (car l-s))
		       project)))

(: frame-append (Frame (U Column Columns Frame) -> Frame))
(define (frame-append frame cols)
  (cond 
   ((Frame? cols)
    (new-frame (append (frame-explode frame) (frame-explode cols))))
   ((list? cols)
    (new-frame (append (frame-explode frame) cols)))
   (else
    (new-frame (append (frame-explode frame) (list cols))))))
