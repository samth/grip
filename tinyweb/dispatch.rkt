;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; REST dispatch for HTTP ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#lang typed/racket/base

 (provide
  DispatchTree dispatch)
;;  rest-resource)

(require 
 (only-in (planet rpr/httpclient:1/http/http11)
	  RequestHeader RequestHeader-request
	  RequestLine-path)
 (only-in (planet rpr/httpclient:1/uri)
	  parse-http-path)
 (only-in (planet rpr/httpclient:1/uri/path)
	  path-split)
 (only-in "log.rkt"
	  www-log))

(define-type Handler (RequestHeader (Listof String) Input-Port Output-Port -> Void))

(define-type SimpleDispatchEntry (List String Handler))
(define-type ComplexDispatchEntry (Rec CE (Pair String (Pair Handler (Listof (U SimpleDispatchEntry CE))))))

(define-type DispatchTree (Listof (U SimpleDispatchEntry ComplexDispatchEntry)))

(define-syntax rest-resource
  (syntax-rules ()
    (( _ (method-sym handler-proc) ...)
     (lambda (request input-port out-port path-remainder)
       (let ((method (string->symbol (car (parse-http-response-line (car request))))))
	 (case method  ;; method
	   ((method-sym) (handler-proc request input-port path-remainder)) ...
	   (else (error-rest-resource method #f))))))
    (( _ (method-sym handler-proc) ... error-proc)
     (lambda (request input-port out-port path-remainder)
       (let ((method (string->symbol (car (parse-http-response-line (car request))))))
	 (case method
	   ((method-sym) (handler-proc request input-port path-remainder)) 
	   ...
	   (else  (error-proc input-port out-port path-remainder))))))))

;; (: rest-resource-error (Symbol Input-Port Output-Port String))
;; (define (rest-resource-error method input-port output-port path-remainder)
;;   (display "Error: Resource does not support ")
;;   (display method)
;;   (newline)
;;   (let ((options (lambda () 'OPTIONS))
;; 	(get     (lambda () 'GET))
;; 	(head    (lambda () 'HEAD))
;; 	(put     (lambda () 'PUT))
;; 	(post    (lambda () 'POST))
;; 	(delete  (lambda () 'DELETE))
;; 	(trace   (lambda () 'TRACE))
;; 	(connect (lambda () 'CONNECT)))
;;     (case method
;;       ((OPTIONS)
;;        (options))
;;       ((GET)
;;        (get))
;;       ((HEAD)
;;        (head))
;;       ((PUT)
;;        (put))
;;       ((POST)
;;        (post))
;;       ((DELETE)
;;        (delete))
;;       ((TRACE)
;;        (trace))
;;       ((CONNECT)
;;        (connect)))))

;; (define-syntax rest-resource
;;   (syntax-rules ()
;;     (( _ (method-sym handler-proc) ...)
;;      (rest-resource (method-sym handler-proc) ... rest-resource-error))
;;     (( _ (method-sym handler-proc) ... error-proc)
;;      (lambda (request input-port output-port path-remainder)
;;        (let ((method (string->symbol (car (parse-request-line (car request))))))
;; 	 (case method
;; 	   ((method-sym) (handler-proc request input-port output-port path-remainder)) 
;; 	   ...
;; 	   (else  (error-proc method input-port output-port path-remainder))))))))

(: node-segment (ComplexDispatchEntry -> String))
(define node-segment
  (lambda (node)
    (car node)))

(: node-dispatch (ComplexDispatchEntry -> Handler))
(define node-dispatch
  (lambda (node)
    (cadr node)))

(: node-children (ComplexDispatchEntry -> (Listof (U SimpleDispatchEntry ComplexDispatchEntry))))
(define node-children
  (lambda (node)
    (cddr node)))

(: dispatch-path-on-tree ((Listof String) DispatchTree ComplexDispatchEntry -> (Values Handler (Listof String))))
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
    (let ((path (path-split http-path)))
      ;; (www-log "URI Path: ~s~%" path)
      (let-values (((resource-handler path-remainder) (dispatch-path-on-tree path dispatch-tree (car dispatch-tree))))
	;; (www-log "Handler: ~s~% Remainder: ~s~%" resource-handler remainder)
	(resource-handler request path-remainder input-port output-port )))))

