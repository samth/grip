#lang typed/racket/base

(provide 
 flseq
 flvector-print)

(require racket/flonum
         racket/pretty
         (only-in "settings.rkt" 
                  Settings-decimals
                  Settings-max-output
                  settings))

;; Not for real.  Sucks I know, it's just to generate test data.
(: flseq (Float Float Float -> FlVector))
(define (flseq start stop by)
  (if (< stop start)
      (flseq stop start by)
      (let* ((span (assert (add1 (inexact->exact (round (/ (- stop start) by))) )
                           exact-integer?))
             (v (make-flvector span)))
        (do ((i 0 (add1 i))
             (x start (+ x by)))
          ((> x stop) v)          
          (flvector-set! v i x)))))          
                                               
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
