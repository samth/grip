#lang racket/base

(provide web-server)

(require
 racket/tcp
 (only-in (planet knozama/webkit:1/web/uri)
	  http-path-path
	  parse-http-path)
 (only-in (planet knozama/webkit:1/web/uri/path)
	  path-split)
 (only-in (planet knozama/webkit:1/web/http/http11)
	  parse-http-response-line
	  request-line-path
	  http-header-from-socket-input-port)
 (only-in "log.rkt"
	  www-log)
 (only-in "dispatch.rkt"
	  dispatch))

(define http-server-task
  (lambda (dispatch-tree port)
    (let ((s (tcp-listen port 25 #t)))
      ;;(socket/nonblock (socket-representation-sock s))
      (let accept-loop ()
	(let-values (((inp outp) (tcp-accept s)))
	  (www-log "Accept: ~s ~%" s)
	  (flush-output (current-output-port))
	  (thread (lambda ()
		    (call-with-exception-handler
		     (lambda (e)
		       (displayln e)
		       ((error-display-handler) "Error in handling request." e)
		       (close-input-port inp)
		       (close-output-port outp))
		     (lambda ()
		       (http-service-task dispatch-tree inp outp)))))
	  (accept-loop))))))

(define http-service-task
  (lambda (dispatch-tree inp outp)
    ;; (www-log  "Got in port ~s~%" in-port)
    (let ((request (http-header-from-socket-input-port inp)))
      (www-log "Request ~s~%" request)
      (dispatch request inp outp dispatch-tree)
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

