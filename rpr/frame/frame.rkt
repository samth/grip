#lang typed/racket/base

(provide:
 [frame-explode (Frame [#:project LabelProjection] -> (Listof (Pair Label Series)))]
 [frame-description (Frame [#:project LabelProjection] -> FrameDescription)])

(provide
 Frame
 (struct-out FrameDescription)
 frame-series
 frame-names frame-dim 
 frame-cseries frame-nseries
 frame-series
 new-frame)

(require 
 (only-in racket/set
	  set-empty? set-member?
	  list->set)
 (only-in "types.rkt"
          Dim Dim-rows Dim-cols)
 (only-in "indexed-series.rkt"
	  label-sort-positional
          Label LabelProjection LabelIndex LabelIndex-index
          GSeries 
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

;; A frame is map of series.
(struct: Frame LabelIndex ([series : (Vectorof Series)]))

(struct: FrameDescription ([dimensions : Dim]
                           [series : (Listof SeriesDescription)]) #:transparent)


(: new-frame ((Listof (Pair Symbol Series)) -> Frame))
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
  (let ((index (build-index-from-labels ((inst map Label (Pair Label Series)) 
                                         (inst car Label Series) cols)))        
        (data (apply vector ((inst map Series (Pair Label Series)) cdr cols))))
    (Frame index data)))

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

(: frame-explode (Frame [#:project LabelProjection] -> (Listof (Pair Label Series))))
(define (frame-explode frame #:project [project '()])
  
  (let ((labeling (label-sort-positional frame))
	(series (Frame-series frame)))
    (projection-filter (for/list: : (Listof (Pair Label Series))
				  ([label labeling])
				  (cons (car label)
					(vector-ref series (cdr label))))
		       (λ: ((l-s : (Pair Label Series))) ;; need to assist TR here.
			   (car l-s))
		       project)))

