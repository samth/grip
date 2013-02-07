;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; REST dispatch for HTTP ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#lang typed/racket/base

(provide
 DispatchTree Dispatcher dispatch
 rest-resource)

(require 
 (only-in "../httpclient/http/http11.rkt"
	  Method RequestHeader RequestHeader-request
	  RequestLine-path RequestLine-method)
 (only-in "../httpclient/uri.rkt"
          parse-http-path)
 (only-in "../httpclient/uri/path.rkt"
          uri-path-split)
 (only-in "log.rkt"
	  www-log))

(define-type Dispatcher (RequestHeader (Listof String) Input-Port Output-Port -> Void))

(define-type SimpleDispatchEntry (List String Dispatcher))
(define-type ComplexDispatchEntry (Rec CE (Pair String (Pair Dispatcher (Listof (U SimpleDispatchEntry CE))))))

(define-type DispatchTree (Listof (U SimpleDispatchEntry ComplexDispatchEntry)))

(: rest-resource-error (Method Input-Port Output-Port (Listof String) -> Void))
(define (rest-resource-error method input-port output-port path-remainder)
  (display "Error: Resource does not support ")
  (display method)
  (newline)
  (let ((options (λ () 'OPTIONS (void)))
	(get     (λ () 'GET (void)))
	(head    (λ () 'HEAD (void)))
	(put     (λ () 'PUT (void)))
	(post    (λ () 'POST (void)))
	(delete  (λ () 'DELETE (void)))
	(trace   (λ () 'TRACE (void)))
	(connect (λ () 'CONNECT (void))))
    (case method
      ((OPTIONS) (options))
      ((GET)     (get))
      ((HEAD)    (head))
      ((PUT)     (put))
      ((POST)    (post))
      ((DELETE)  (delete))
      ((TRACE)   (trace))
      ((CONNECT) (connect)))))

(define-syntax rest-resource
  (syntax-rules ()
    (( _ (method-sym handler-proc) ...)
     (rest-resource (method-sym handler-proc) ... rest-resource-error))
    (( _ (method-sym handler-proc) ... error-proc)
     (lambda (request path-remainder input-port output-port)
       (let ((method (RequestLine-method (RequestHeader-request request))))
	 (case method 
	   ((method-sym) (handler-proc request path-remainder input-port output-port)) 
	   ...
	   (else  (error-proc method input-port output-port path-remainder))))))))

(: node-segment (ComplexDispatchEntry -> String))
(define node-segment
  (lambda (node)
    (car node)))

(: node-dispatch (ComplexDispatchEntry -> Dispatcher))
(define node-dispatch
  (lambda (node)
    (cadr node)))

(: node-children (ComplexDispatchEntry -> (Listof (U SimpleDispatchEntry ComplexDispatchEntry))))
(define node-children
  (lambda (node)
    (cddr node)))

(: dispatch-path-on-tree ((Listof String) DispatchTree ComplexDispatchEntry -> (Values Dispatcher (Listof String))))
(define (dispatch-path-on-tree path node-list mnode)
  (if (or (null? node-list)
	  (null? path))
      (begin
	;; (www-log "Handler: ~s Remainder: ~s~%" (node-dispatch mnode) path)
	(values (node-dispatch mnode) path))
      (let ((node (car node-list)))
	(if (string=? (car path) (node-segment node))
	    (dispatch-path-on-tree (cdr path) (node-children node) node)
	    (dispatch-path-on-tree path (cdr node-list) mnode)))))

(: dispatch (RequestHeader Input-Port Output-Port DispatchTree -> Void))
(define (dispatch request input-port output-port dispatch-tree)
  (let ((http-path (RequestLine-path (RequestHeader-request request))))
    ;; (www-log "Full Request: ~s~%" request)
    ;; (www-log "HTTP Path: ~s~%" http-path)
    (let ((path (uri-path-split http-path)))
      ;; (www-log "URI Path: ~s~%" path)
      (let-values (((resource-handler path-remainder) (dispatch-path-on-tree path dispatch-tree (car dispatch-tree))))
	;; (www-log "Handler: ~s~% Remainder: ~s~%" resource-handler remainder)
	(resource-handler request path-remainder input-port output-port )))))

