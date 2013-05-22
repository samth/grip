#lang typed/racket/base

(provide:
 [nseries-ref (NSeries Index -> Float)]
 [nseries-label-ref (NSeries Label -> Float)]
 [nseries-count (NSeries -> Index)])

(provide
 flvector-print
 (struct-out NSeries)
 mkNSeries map/NSeries
 map/series->NSeries map/NSeries->series)

(require 
 racket/flonum
 (only-in "settings.rkt" 
          Settings-decimals
          Settings-max-output
          settings)
 (only-in "indexed-series.rkt"  
          label-index label->idx
          build-index-from-labels
          Label SIndex
          GSeries GSeries-data
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

(: writer-NSeries (NSeries Output-Port Boolean -> Void))
(define (writer-NSeries series port mode)
  (let* ([data (NSeries-data series)]
         [len (flvector-length data)])
    (displayln (format "(NSeries #:length ~s)" len) port)))    

;; An NSeries is an optimized Series for computation over vectors of Float
;; i.e., NSeries should be faster then (Series Float)
(struct: NSeries LabelIndex ([data : FlVector])
;;  #:methods gen:custom-write [(define write-proc writer-NSeries)]
	 )

(: mkNSeries (FlVector (Option (U (Listof Label) SIndex)) -> NSeries))
(define (mkNSeries data labels)

  (: check-mismatch (SIndex -> Void))
  (define (check-mismatch index)
    (unless (eq? (flvector-length data) (hash-count index))
      (let ((k (current-continuation-marks)))
	(raise (make-exn:fail:contract "Cardinality of a Series' data and labels must be equal" k))))
    (void))

  (if (hash? labels)
     (begin
       (check-mismatch labels)
       (NSeries labels data))
     (if labels
	(let ((index (build-index-from-labels labels)))
	  (check-mismatch index)
	  (NSeries index data))
	(NSeries #f data))))

(: nseries-ref (NSeries Index -> Float))
(define (nseries-ref series idx)
  (flvector-ref (NSeries-data series) idx))

(: nseries-label-ref (NSeries Label -> Float))
(define (nseries-label-ref series label)
  (nseries-ref series (label->idx series label)))

(: nseries-count (NSeries -> Index))
(define (nseries-count nseries)
  (flvector-length (NSeries-data nseries)))

(: map/NSeries (NSeries (Float -> Float) -> NSeries))
(define (map/NSeries series fn)  
  (let ((old-data (NSeries-data series))
      (new-data (make-flvector (flvector-length (NSeries-data series)))))
    (let ((len (flvector-length old-data)))
      (let loop ((idx 0))
	(if (< idx len)
	   (begin
	     (flvector-set! new-data idx (fn (flvector-ref old-data idx)))
	     (loop (add1 idx)))
	   (void))))
    (NSeries (LabelIndex-index series) new-data)))

(: map/NSeries->series (All (A) NSeries (Float -> A) -> (GSeries A)))
(define (map/NSeries->series series fn)
  (let*: ((old-data : FlVector (NSeries-data series))
	(new-data : (Vectorof A) (build-vector (flvector-length old-data) 
					       (λ: ((idx : Integer)) 
                                                 (fn (flvector-ref old-data idx))))))
       (GSeries (LabelIndex-index series) new-data)))

(: map/series->NSeries (All (A) (GSeries A) (A -> Float) -> NSeries))
(define (map/series->NSeries series fn)
  (let* ((old-data (GSeries-data series))
         (len (vector-length old-data))
         (new-data (make-flvector len)))
    (let loop ((idx 0))
      (if (< idx len)
          (begin
            (flvector-set! new-data idx (fn (vector-ref old-data idx)))
            (loop (add1 idx)))
          (NSeries (LabelIndex-index series) new-data)))))
