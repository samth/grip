#lang typed/racket/base

(provide:
 [show-settings (-> Void)])

(provide settings          
         (struct-out Settings))

(struct: Settings ([decimals : Integer]
                   [max-output : Integer]))

(define default-decimals 6)
(define default-max-output 1000)

(: settings (Parameterof Settings))
(define settings (make-parameter 
                  (Settings default-decimals
                            default-max-output)))
(define (show-settings)
  
  (define (sf k v)
    (format "~a: ~a" k v))
    
  (let ((s (settings)))
    (displayln (sf "Decimals" (Settings-decimals s)))
    (displayln (sf "Max Lines" (Settings-max-output s)))))