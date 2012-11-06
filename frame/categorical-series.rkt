#lang typed/racket/base

(provide
 (struct-out CategoricalSeries))
 ;;writer-CategoricalSeries)

(provide:
 [CategoricalSeries->SIndex (CategoricalSeries -> SIndex)])

(require 
 (only-in "series.rkt"
          SIndex
          LabelIndex))

;; Categorical Series
;; Encoded as an array of integer values with an associated nominal.
;; Custom Structure Writer 
;; See 12.8 Printer Extensions in Racket doc.
(: writer-CategoricalSeries (CategoricalSeries Output-Port Boolean -> Void))
(define (writer-CategoricalSeries series port mode)  
  (let* ([data (CategoricalSeries-data series)]
         [nominals (CategoricalSeries-nominals series)]
         [len (vector-length data)])
    (do ([i 0 (add1 i)])
      ((>= i len) (void))
      (displayln  (vector-ref nominals (vector-ref data i))))))

(struct: CategoricalSeries LabelIndex ([data     : (Vectorof Index)] 
                                       [nominals : (Vectorof Symbol)])
  #:transparent)
  ;; #:methods gen:custom-write [(define write-proc writer-CategoricalSeries)])

(: CategoricalSeries->SIndex (CategoricalSeries -> SIndex))
(define (CategoricalSeries->SIndex cs)
  
  (: sindex SIndex)  
  (define sindex (make-hash))
  
  (let* ((noms (CategoricalSeries-nominals cs))
         (len (vector-length noms)))
    (do ([i 0 (add1 i)])
      ([>= i len] sindex)
      (when (index? i)     
        (hash-set! sindex (vector-ref noms i) i)))))