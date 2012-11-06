#lang typed/racket/base

(provide:
 [flvector-print (FlVector -> Void)]
 [generate-NumericSeries (Float Float [#:by Float] -> NumericSeries)])

(require
 racket/flonum
 (only-in "numeric-series.rkt"
          NumericSeries) 
 (only-in "settings.rkt" 
          Settings-decimals
          Settings-max-output
          settings))

(: generate-NumericSeries (Float Float [#:by Float] -> NumericSeries))
(define (generate-NumericSeries start stop #:by [step 1.0])
  (if (< stop start)
      (generate-NumericSeries stop start #:by step)
      (let* ((span (assert (add1 (inexact->exact (round (/ (- stop start) step))))
                           exact-integer?))
             (v (make-flvector span)))
        (do ((i 0 (add1 i))
             (x start (+ x step)))
          ((>= i span) v)          
          (flvector-set! v i x))
        (NumericSeries #f v))))

(: flvector-print (FlVector -> Void))
(define (flvector-print flv)  
  (let ((len (flvector-length flv))
        (out (current-output-port))
        (decs (Settings-decimals (settings)))
        (max-output (Settings-max-output (settings))))        
    (displayln (string-append "F." (number->string (Settings-decimals (settings)))))
    (if (zero? len)
        (displayln "[ ]")
        (begin
          (display "[ ")
          (do ((i 0 (add1 i)))
            ((>= i len) (void))
            (display (real->decimal-string (flvector-ref flv i) decs))
            (display " "))
	  (display "]")))))