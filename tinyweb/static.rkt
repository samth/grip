#lang racket/base

;; (provide
;;  www-root-set!
;;  serve-static-resource
;;  static-resource)

;; (require
;;  (only-in (planet knozama/webkit:1/web/http/http11)
;; 	  http-send-response)
;;  (only-in (planet knozama/common:1/text/util)
;; 	  weave-string-separator)
;;  (only-in "util.rkt"
;; 	  normalize-path)
;;  (only-in "dispatch.rkt"
;; 	  rest-resource)
;;  (only-in "log.rkt"
;; 	  www-log))

;; (define *www-root* "/dev/null")

;; (define *www-default* "index.html")	      ;; name to use if request ends in / 

;; (define crlf
;;   (string->bytes/utf-8 "\r\n"))

;; (define www-root-set!
;;   (lambda (path)
;;     (set! *www-root* path)))

;; (define static-default-age
;;   (string-append "max-age=" (number->string (* 60 60 24)))) ;; one day

;; (define serve-static-resource
;;   (lambda (path-segs output-port)
;;     (let ((path (normalize-path path-segs)))
;;       (when path
;; 	(let ((full-path(string-append *www-root* "/" (weave-string-separator "/" path))))
;; 	  (www-log "GET - ~s~%" full-path)
;; 	  (call-with-exception-handler 
;; 	   (lambda (e)
;; 	     (www-log "Unexpected exception ~s~%" e)
;; 	     (www-log "ERROR - Failed to send static resource ~s ~%" full-path)
;; 	     (let ((bad-path (weave-string-separator "/" path)))
;; 	       (www-log "BAD PATH - ~s ~%" bad-path)
;; 	       (write output-port (string-append "Request for static resource failed!")))
;; 	     #f)
;; 	   (lambda ()
;; 	     (let ((ip (open-input-file full-path)))
;; 	       (http-send-response "200 OK" `(("Content-Type"  . "text/html; charset=UTF8")
;; 					      ("Cache-Control" . ,static-default-age))
;; 				   output-port ip 0)
;; 	       (close-input-port ip))
;; 	     #t)))))))

;; (define static-resource
;;   (rest-resource
;;    (GET (lambda (request remainder input-port output-port)
;; 	  (let ((path-remainder (car remainder)))
;; 	    (serve-static-resource path-remainder output-port))))))

