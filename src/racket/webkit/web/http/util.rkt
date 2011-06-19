#lang typed/racket

(provide
 ok-response?)

(require
 (only-in "http11.rkt"
	  HTTP-Resp-Header
	  response-line-code
	  http-response-from-headers))

;; HTTP 200 received
(: ok-response? ((Option HTTP-Resp-Header) -> Boolean))
(define (ok-response? resp-info)
  (if resp-info
     (let ((http-resp (http-response-from-headers resp-info)))
       (if http-resp
	  (let ((code (response-line-code http-resp)))
	    (if code
	       (string=? code "200")
	       #f))
	  #f))
     #f))
       

  
