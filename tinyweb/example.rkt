#lang typed/racket/base

(provide
 main)

(require
 (only-in "../httpclient/http/http11.rkt"
          http-send-response
          RequestHeader
          RequestLine-method)
 (only-in "../format/xml/sxml.rkt"
          sxml->html)		  
 (only-in "dispatch.rkt"
          DispatchTree Dispatcher rest-resource)
 (only-in "server.rkt"
          webserver))

(:  hello-html (-> (Listof Any)))
(define (hello-html)
  '(html
    (heading
     (title "Hello"))
    (body
     (p "Hello World."))))

(: hello-resource Dispatcher)
(define hello-resource
  (rest-resource
   (GET (λ: ((request : RequestHeader)
	     (path-remainder : (Listof String))		  
	     (input-port : Input-Port)
	     (output-port : Output-Port))
	    (let ((str-out (open-output-string)))
	      (sxml->html (hello-html) str-out)
	      (http-send-response "200 OK" 
				  '() 
				  output-port
				  (open-input-bytes (string->bytes/utf-8 (get-output-string str-out)))
				  0))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Launch and answer an HTTP GET at the following URLs
;; http://localhost:8088/
;; http://localhost:8088/hello
;; http://localhost:8088/hello/there
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(: dispatch-tree (-> DispatchTree))
(define (dispatch-tree)
  `((""  ,hello-resource
     ("hello" ,hello-resource
      ("there" ,hello-resource)))))

(: main (-> Void))
(define (main)
  (webserver dispatch-tree 8088))
