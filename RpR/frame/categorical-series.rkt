#lang typed/racket/base

(provide
 (struct-out CSeries))
 ;;writer-CategoricalSeries)

(provide:
 [CSeries->SIndex (CSeries -> SIndex)]
 [cseries-count   (CSeries -> Nonnegative-Integer)])

(require 
 (only-in "series.rkt"
          SIndex
          LabelIndex))

;; Categorical Series
;; Encoded as an array of integer values with an associated nominal.
;; Custom Structure Writer 
;; See 12.8 Printer Extensions in Racket doc.
(: writer-CSeries (CSeries Output-Port Boolean -> Void))
(define (writer-CSeries series port mode)  
  (let* ([data (CSeries-data series)]
         [nominals (CSeries-nominals series)]
         [len (vector-length data)])
    (do ([i 0 (add1 i)])
      ((>= i len) (void))
      (displayln  (vector-ref nominals (vector-ref data i))))))

(struct: CSeries LabelIndex ([data     : (Vectorof Index)] 
                             [nominals : (Vectorof Symbol)])
  #:transparent)
;; #:methods gen:custom-write [(define write-proc writer-CategoricalSeries)])

(: CSeries->SIndex (CSeries -> SIndex))
(define (CSeries->SIndex cs)
  
  (: sindex SIndex)  
  (define sindex (make-hash))
  
  (let* ((noms (CSeries-nominals cs))
         (len (vector-length noms)))
    (do ([i 0 (add1 i)])
      ([>= i len] sindex)
      (when (index? i)     
        (hash-set! sindex (vector-ref noms i) i)))))

(: cseries-count (CSeries -> Nonnegative-Integer))
(define (cseries-count series)
  (vector-length (CSeries-data series)))