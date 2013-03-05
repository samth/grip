#lang typed/racket/base

(provide
 (struct-out NSeriesBuilder))

(provide:
 [new-NSeriesBuilder (case->
		      (-> NSeriesBuilder)
		      (Index -> NSeriesBuilder))]
 [append-NSeriesBuilder   (NSeriesBuilder (U Float String) -> Void)]
 [complete-NSeriesBuilder (NSeriesBuilder -> NSeries)])

(require
 racket/flonum
 (only-in "../frame/numeric-series.rkt"
          NSeries))
(struct: NSeriesBuilder ([index  : Index]
			 [data : FlVector]) #:mutable)

(define base-len 512)

(: new-NSeriesBuilder (case->
		       (-> NSeriesBuilder)
		       (Index -> NSeriesBuilder)))
(define (new-NSeriesBuilder [len base-len])
  (NSeriesBuilder 0 (make-flvector len +nan.0)))

(: append-NSeriesBuilder (NSeriesBuilder (U Float String) -> Void))
(define (append-NSeriesBuilder builder flo/str-value)
  
  (define-syntax bump
    (syntax-rules ()
      [(bump x)
       (assert (add1 x) index?)]))
  
  (define (bump-index)
    (let ((idx (NSeriesBuilder-index builder)))
      (set-NSeriesBuilder-index! builder (bump idx))
      idx))
  
  (: extend-data (-> Void))
  (define (extend-data)
    (let* ((data (NSeriesBuilder-data builder))
	   (curr-len (flvector-length data))
	   (new-len  (assert (inexact->exact (round (* 1.5 curr-len))) exact-integer?)))
      (let: ((new-data : FlVector (make-flvector new-len +nan.0)))
	    (do ([idx 0 (add1 idx)])  ;; RACKET REQUEST FOR RUNTIME SUPPORT FOR COPY
		([>= idx curr-len] (set-NSeriesBuilder-data! builder new-data))
	      (flvector-set! new-data idx (flvector-ref data idx))))))
  
  (if (< (NSeriesBuilder-index builder)         
         (flvector-length (NSeriesBuilder-data builder)))
      (let ((num (if (string? flo/str-value)
		     (let ((num (string->number flo/str-value)))
		       (if num (assert (exact->inexact num) flonum?) +nan.0))
		     flo/str-value)))
        (flvector-set! (NSeriesBuilder-data builder)
                       (bump-index)
                       num))
      (begin
        (extend-data)       
        (append-NSeriesBuilder builder flo/str-value))))

(: complete-NSeriesBuilder (NSeriesBuilder -> NSeries))
(define (complete-NSeriesBuilder builder)  
  (let* ((data (NSeriesBuilder-data builder))
         (len (NSeriesBuilder-index builder)))
    (NSeries #f (flvector-copy data 0 len))))
