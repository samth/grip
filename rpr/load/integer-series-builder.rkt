#lang typed/racket/base

(provide
 (struct-out ISeriesBuilder))

(provide:
 [new-ISeriesBuilder (case-> 
		      (-> ISeriesBuilder)
		      (Index -> ISeriesBuilder))]
 [append-ISeriesBuilder   (ISeriesBuilder (U Fixnum String) -> Void)]
 [complete-ISeriesBuilder (ISeriesBuilder -> ISeries)])

(require
 racket/fixnum
 (only-in racket/vector
	  vector-copy)
 (only-in "../frame/integer-series.rkt"
          ISeries))

(struct: ISeriesBuilder ([index  : Index]
			 [data : (Vectorof Fixnum)]) #:mutable)

(define base-len 512)

(: new-ISeriesBuilder (case-> 
		       (-> ISeriesBuilder)
		       (Index -> ISeriesBuilder)))
(define (new-ISeriesBuilder [len base-len])
  (ISeriesBuilder 0 (make-vector len 0)))

(: append-ISeriesBuilder (ISeriesBuilder (U Fixnum String) -> Void))
(define (append-ISeriesBuilder builder int/str-value)
  
  (define-syntax bump
    (syntax-rules ()
      [(bump x)
       (assert (add1 x) index?)]))
  
  (define (bump-index)
    (let ((idx (ISeriesBuilder-index builder)))
      (set-ISeriesBuilder-index! builder (bump idx))
      idx))
  
  (: extend-data (-> Void))
  (define (extend-data)
    (let* ((data (ISeriesBuilder-data builder))
	   (curr-len (vector-length data))
	   (new-len  (assert (inexact->exact (round (* 1.5 curr-len))) exact-integer?)))
      (let: ((new-data : (Vectorof Fixnum) ((inst make-vector Fixnum) new-len 0)))
	    (do ([idx 0 (add1 idx)])  ;; RACKET REQUEST FOR RUNTIME SUPPORT FOR COPY
		([>= idx curr-len] (set-ISeriesBuilder-data! builder new-data))
	      (vector-set! new-data idx (vector-ref data idx))))))
  
  (if (< (ISeriesBuilder-index builder)         
         (vector-length (ISeriesBuilder-data builder)))
      (let ((num (if (string? int/str-value)
		     (let ((num (string->number int/str-value)))
		       (if num (assert num fixnum?) 0))
		     int/str-value)))
        (vector-set! (ISeriesBuilder-data builder)
		     (bump-index)
		     num))
      (begin
        (extend-data)       
        (append-ISeriesBuilder builder int/str-value))))

(: complete-ISeriesBuilder (ISeriesBuilder -> ISeries))
(define (complete-ISeriesBuilder builder)  
  (let* ((data (ISeriesBuilder-data builder))
         (len (ISeriesBuilder-index builder)))
    (ISeries #f (vector-copy data 0 len))))
