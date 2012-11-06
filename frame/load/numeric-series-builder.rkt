#lang typed/racket/base

(provide
 (struct-out NumericSeriesBuilder))

(provide:
 [mkNumericSeriesBuilder        (-> NumericSeriesBuilder)]
 [append-NumericSeriesBuilder   (NumericSeriesBuilder String -> Void)]
 [complete-NumericSeriesBuilder (NumericSeriesBuilder -> NumericSeries)])

(require
 racket/flonum
 (only-in "../frame/numeric-series.rkt"
          NumericSeries))
(struct: NumericSeriesBuilder ([index  : Index]
                               [data : FlVector]) #:mutable #:transparent)

(: mkNumericSeriesBuilder (-> NumericSeriesBuilder))
(define (mkNumericSeriesBuilder)
  (define base-len 32)
  (NumericSeriesBuilder 0 (make-flvector base-len +nan.0)))

(: append-NumericSeriesBuilder (NumericSeriesBuilder String -> Void))
(define (append-NumericSeriesBuilder builder str)
  
  (define-syntax bump
    (syntax-rules ()
      [(bump x)
       (assert (add1 x) index?)]))
  
  (define (bump-index)
    (let ((idx (NumericSeriesBuilder-index builder)))
      (set-NumericSeriesBuilder-index! builder (bump idx))
      idx))
  
  (: extend-data (-> Void))
  (define (extend-data)
    (let* ((data (NumericSeriesBuilder-data builder))
	 (curr-len (flvector-length data))
	 (new-len  (assert (inexact->exact (round (* 1.5 curr-len))) exact-integer?)))
      (let: ((new-data : FlVector (make-flvector new-len +nan.0)))
        (do ([idx 0 (add1 idx)])  ;; RACKET REQUEST FOR RUNTIME SUPPORT FOR COPY
	    ([>= idx curr-len] (set-NumericSeriesBuilder-data! builder new-data))
          (flvector-set! new-data idx (flvector-ref data idx))))))
  
  (if (< (NumericSeriesBuilder-index builder)         
         (flvector-length (NumericSeriesBuilder-data builder)))
      (let ((num (let ((num (string->number str)))
                   (if num (assert (exact->inexact num) flonum?) +nan.0))))        
        (flvector-set! (NumericSeriesBuilder-data builder)
                       (bump-index)
                       num))
      (begin
        (extend-data)       
        (append-NumericSeriesBuilder builder str))))

(: complete-NumericSeriesBuilder (NumericSeriesBuilder -> NumericSeries))
(define (complete-NumericSeriesBuilder builder)  
  (let* ((data (NumericSeriesBuilder-data builder))
         (len (NumericSeriesBuilder-index builder)))
    (NumericSeries #f (flvector-copy data 0 len))))
