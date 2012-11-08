#lang typed/racket/base

(provide:
 [flvector-print (FlVector -> Void)]
 [generate-NSeries (Float Float [#:by Float] -> NSeries)])

(require
 racket/flonum
 (only-in "numeric-series.rkt"
          NSeries) 
 (only-in "settings.rkt" 
          Settings-decimals
          Settings-max-output
          settings))

(: generate-NSeries (Float Float [#:by Float] -> NSeries))
(define (generate-NSeries start stop #:by [step 1.0])
  (if (< stop start)
      (generate-NSeries stop start #:by step)
      (let* ((span (assert (add1 (inexact->exact (round (/ (- stop start) step))))
                           exact-integer?))
             (v (make-flvector span)))
        (do ((i 0 (add1 i))
             (x start (+ x step)))
          ((>= i span) v)          
          (flvector-set! v i x))
        (NSeries #f v))))

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