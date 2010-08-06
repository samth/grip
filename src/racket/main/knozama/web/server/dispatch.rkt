;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; REST dispatch for HTTP ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#lang racket

(provide
 dispatch
 rest-resource)

(require 
 (only-in knozama/web/server/log
	  www-log)
 (only-in knozama/web/http/http
	  parse-request-line
	  parse-http-response-line)
 (only-in knozama/web/uri
	  parse-http-path)
 (only-in knozama/web/uri/path
	  path-split))

;;  (define-syntax rest-resource
;;    (syntax-rules ()
;;      (( _ (method-sym handler-proc) ...)
;;       (lambda (request input-port out-port path-remainder)
;; 	(let ((method (string->symbol (car (parse-http-response-line (car request))))))
;; 	  (case method  ;; method
;; 	    ((method-sym) (handler-proc request input-port path-remainder)) ...
;; 	    (else (error-rest-resource method #f))))))
;;      (( _ (method-sym handler-proc) ... error-proc)
;;       (lambda (request input-port out-port path-remainder)
;; 	(let ((method (string->symbol (car (parse-http-response-line (car request))))))
;; 	  (case method
;; 	    ((method-sym) (handler-proc request input-port path-remainder)) 
;; 	    ...
;; 	    (else  (error-proc input-port out-port path-remainder))))))))

(define rest-resource-error
  (lambda (method input-port output-port path-remainder)
    (display "Error: Resource does not support ")
    (display method)
    (newline)
    (let ((options (lambda () 'OPTIONS))
	(get     (lambda () 'GET))
	(head    (lambda () 'HEAD))
	(put     (lambda () 'PUT))
	(post    (lambda () 'POST))
	(delete  (lambda () 'DELETE))
	(trace   (lambda () 'TRACE))
	(connect (lambda () 'CONNECT)))
      (case method
	((OPTIONS)
	 (options))
	((GET)
	 (get))
	((HEAD)
	 (head))
	((PUT)
	 (put))
	((POST)
	 (post))
	((DELETE)
	 (delete))
	((TRACE)
	 (trace))
	((CONNECT)
	 (connect))))))

(define-syntax rest-resource
  (syntax-rules ()
    (( _ (method-sym handler-proc) ...)
     (rest-resource (method-sym handler-proc) ... rest-resource-error))
    (( _ (method-sym handler-proc) ... error-proc)
     (lambda (request input-port output-port path-remainder)
       (let ((method (string->symbol (car (parse-request-line (car request))))))
	 (case method
	   ((method-sym) (handler-proc request input-port output-port path-remainder)) 
	   ...
	   (else  (error-proc method input-port output-port path-remainder))))))))

(define node-segment
  (lambda (node)
    (car node)))

(define node-dispatch
  (lambda (node)
    (cadr node)))

(define node-children
  (lambda (node)
    (cddr node)))

(define dispatch-path-on-tree
  (lambda (path node-list mnode)
    (displayln "Path")
    (displayln path)
    (displayln "====")
    (displayln node-list)
    (displayln mnode)
    (displayln "---")
    (if (or (null? node-list)
	  (null? path))
       (begin
	 (www-log "Handler: ~s Remainder: ~s~%" (node-dispatch mnode) path)
	 (values (node-dispatch mnode) path))
       (let ((node (car node-list)))
	 (if (string=? (car path) (node-segment node))
	    (dispatch-path-on-tree (cdr path) (node-children node) node)
	    (dispatch-path-on-tree path (cdr node-list) mnode))))))

(define dispatch 
  (lambda (request input-port output-port dispatch-tree)
    (let ((http-path (parse-http-path (cadr (parse-request-line (car request))))))
      (www-log "Full Request: ~s~%" request)
      (www-log "HTTP Path: ~s~%" http-path)
      (let ((path (path-split (car http-path))))
	(www-log "URI Path: ~s~%" path)
	(let-values (((resource-handler path-remainder) (dispatch-path-on-tree path dispatch-tree #f)))	  
	  (let ((remainder (cons path-remainder (cdr http-path))))
	    (www-log "Handler: ~s~% Remainder: ~s~%" resource-handler remainder)
	    (resource-handler request remainder input-port output-port )))))))

