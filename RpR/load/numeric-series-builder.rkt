#lang typed/racket/base

(provide
 (struct-out NSeriesBuilder))

(provide:
 [mkNSeriesBuilder        (-> NSeriesBuilder)]
 [append-NSeriesBuilder   (NSeriesBuilder String -> Void)]
 [complete-NSeriesBuilder (NSeriesBuilder -> NSeries)])

(require
 racket/flonum
 (only-in "../frame/numeric-series.rkt"
          NSeries))
(struct: NSeriesBuilder ([index  : Index]
                               [data : FlVector]) #:mutable #:transparent)

(: mkNSeriesBuilder (-> NSeriesBuilder))
(define (mkNSeriesBuilder)
  (define base-len 32)
  (NSeriesBuilder 0 (make-flvector base-len +nan.0)))

(: append-NSeriesBuilder (NSeriesBuilder String -> Void))
(define (append-NSeriesBuilder builder str)
  
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
      (let ((num (let ((num (string->number str)))
                   (if num (assert (exact->inexact num) flonum?) +nan.0))))        
        (flvector-set! (NSeriesBuilder-data builder)
                       (bump-index)
                       num))
      (begin
        (extend-data)       
        (append-NSeriesBuilder builder str))))

(: complete-NSeriesBuilder (NSeriesBuilder -> NSeries))
(define (complete-NSeriesBuilder builder)  
  (let* ((data (NSeriesBuilder-data builder))
         (len (NSeriesBuilder-index builder)))
    (NSeries #f (flvector-copy data 0 len))))
