#lang racket

 (provide web-server)
 
 (require
  (only-in knozama/std/prelude
	   add1 sub1)
  (only-in knozama/web/uri
	   http-path-path
	   parse-http-path)
  (only-in knozama/web/uri/path
	   path-split)
  (only-in knozama/web/http/http
	   parse-http-response-line
	   request-line-path
	   http-header-from-socket-input-port)
  (only-in knozama/web/server/log
	   www-log)
  (only-in knozama/web/server/dispatch
	   dispatch))
 
 (define http-server-task
   (lambda (dispatch-tree port)
     (let ((s (tcp-listen port)))
       ;;(socket/nonblock (socket-representation-sock s))
       (let accept-loop ()
	 (let-values (((inp outp) (tcp-accept s)))
	   (www-log "Accept: ~s ~%" s)
	   (flush-output (current-output-port))
	   (thread (lambda ()
		     (http-service-task dispatch-tree inp outp)))
	   (accept-loop))))))
 
 (define http-service-task
   (lambda (dispatch-tree inp outp)
     (www-log  "Got in port ~s~%" in-port)
     (let ((request (http-header-from-socket-input-port inp)))
       (www-log "Request ~s~%" request)
       (dispatch request inp outp dispatch-tree)
       ;; (www-log "Socket Close ~s.~%" socket)
       (close-input-port inp)
       (close-output-port outp))))

 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ;; Main routine to launch the server. ;;
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 (define web-server 
   (lambda (configure port)
     (let ((dispatch-tree (configure)))
       (www-log "Starting server on port ~s.~%" port)	
       (www-log "Dispatch Tree ~%")
       (displayln dispatch-tree)
       (call-in-nested-thread (lambda () (http-server-task dispatch-tree port))))))

