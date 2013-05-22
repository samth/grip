#lang typed/racket/base

(provide
 (struct-out Dim))

(: writer-Dim (Dim Output-Port Boolean -> Void))
(define (writer-Dim dim port mode)
  (display (format "(Dim #:rows ~s #:cols ~s)" 
                   (Dim-rows dim)
                   (Dim-cols dim)) port))

(struct: Dim ([rows : Index]
              [cols : Index])
;;	 #:methods gen:custom-write [(define write-proc writer-Dim)]
	 #:transparent)
