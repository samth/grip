#lang typed/racket/base

(provide settings 
         display-settings
         (struct-out Settings))

(struct: Settings ([decimals : Integer]
                   [max-output : Integer]))

(define default-decimals 6)
(define default-max-output 1000)

(: settings (Parameterof Settings))
(define settings (make-parameter 
                  (Settings default-decimals
                            default-max-output)))
(define (display-settings)
  (displayln "Sorry... Fix me."))