#lang typed/racket/base

(provide
 NumericSeries NumericSeries?
 NumericSeries-data
 NumericSeries-length
 mkNumericSeries map/NumericSeries
 NumericSeries-ref NumericSeries-iref
 map/series->NumericSeries map/NumericSeries->series)
 
(require 
 racket/flonum
 (only-in "settings.rkt" 
          Settings-decimals
          Settings-max-output
          settings)
 (only-in "series.rkt"  
          label-index label->idx
          build-index-from-labels
          Label SIndex
          GenericSeries GenericSeries-data
          LabelIndex LabelIndex-index))
         
(: flvector-print (FlVector Output-Port -> Void))
(define (flvector-print flv port)  
  (let ((len (flvector-length flv))
        (out (current-output-port))
        (decs (Settings-decimals (settings)))
        (max-output (Settings-max-output (settings))))        
    (if (zero? len)
        (displayln "[ ]" port)
        (begin
          (display "[ " port)
          (do ((i 0 (add1 i)))
            ((>= i len) (void))
            (let ((num (flvector-ref flv i)))
              (if (eqv? num +nan.0)
                  (display num port)
                  (display (real->decimal-string num decs) port)))
            (display " " port))))
    (display "]" port)))

(: writer-NumericSeries (NumericSeries Output-Port Boolean -> Void))
(define (writer-NumericSeries series port mode)
  (let* ([data (NumericSeries-data series)]
         [len (flvector-length data)])
    (displayln (format "(NumericSeries #:length ~s)" len) port)))    

;; An NumericSeries is an optimized Series for computation over vectors of Float
;; i.e., NumericSeries should be faster then (Series Float)
(struct: NumericSeries LabelIndex ([data : FlVector]))
  ;;#:methods gen:custom-write [(define write-proc writer-NumericSeries)])

(: mkNumericSeries (FlVector (Option (U (Listof Label) SIndex)) -> NumericSeries))
(define (mkNumericSeries data labels)

  (: check-mismatch (SIndex -> Void))
  (define (check-mismatch index)
    (unless (eq? (flvector-length data) (hash-count index))
      (let ((k (current-continuation-marks)))
	(raise (make-exn:fail:contract "Cardinality of a Series' data and labels must be equal" k))))
    (void))

  (if (hash? labels)
     (begin
       (check-mismatch labels)
       (NumericSeries labels data))
     (if labels
	(let ((index (build-index-from-labels labels)))
	  (check-mismatch index)
	  (NumericSeries index data))
	(NumericSeries #f data))))

(: NumericSeries-iref (NumericSeries Index -> Float))
(define (NumericSeries-iref series idx)
  (flvector-ref (NumericSeries-data series) idx))

(: NumericSeries-ref (NumericSeries Label -> Float))
(define (NumericSeries-ref series label)
  (NumericSeries-iref series (label->idx series label)))

(: NumericSeries-length (NumericSeries -> Nonnegative-Integer))
(define (NumericSeries-length nseries)
  (flvector-length (NumericSeries-data nseries)))

(: map/NumericSeries (NumericSeries (Float -> Float) -> NumericSeries))
(define (map/NumericSeries series fn)  
  (let ((old-data (NumericSeries-data series))
      (new-data (make-flvector (flvector-length (NumericSeries-data series)))))
    (let ((len (flvector-length old-data)))
      (let loop ((idx 0))
	(if (< idx len)
	   (begin
	     (flvector-set! new-data idx (fn (flvector-ref old-data idx)))
	     (loop (add1 idx)))
	   (void))))
    (NumericSeries (LabelIndex-index series) new-data)))

(: map/NumericSeries->series (All (A) NumericSeries (Float -> A) -> (GenericSeries A)))
(define (map/NumericSeries->series series fn)
  (let*: ((old-data : FlVector (NumericSeries-data series))
	(new-data : (Vectorof A) (build-vector (flvector-length old-data) 
					       (λ: ((idx : Integer)) 
                                                 (fn (flvector-ref old-data idx))))))
       (GenericSeries (LabelIndex-index series) new-data)))

(: map/series->NumericSeries (All (A) (GenericSeries A) (A -> Float) -> NumericSeries))
(define (map/series->NumericSeries series fn)
  (let* ((old-data (GenericSeries-data series))
         (len (vector-length old-data))
         (new-data (make-flvector len)))
    (let loop ((idx 0))
      (if (< idx len)
          (begin
            (flvector-set! new-data idx (fn (vector-ref old-data idx)))
            (loop (add1 idx)))
          (NumericSeries (LabelIndex-index series) new-data)))))
