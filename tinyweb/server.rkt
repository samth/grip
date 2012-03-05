#lang typed/racket/base

(provide webserver)

(require
 racket/tcp
 (only-in (planet rpr/httpclient:1/uri)
	  http-path-path
	  parse-http-path)
 (only-in (planet rpr/httpclient:1/uri/path)
	  path-split)
 (only-in (planet rpr/httpclient:1/http/http11)
	  read-request-header)
 (only-in "log.rkt"
	  www-log)
 (only-in "dispatch.rkt"
	  DispatchTree dispatch))

(: http-server-task (DispatchTree Integer -> Void))
(define (http-server-task dispatch-tree port)
  (let ((s (tcp-listen port 25 #t)))
    ;;(socket/nonblock (socket-representation-sock s))
    (let accept-loop ()
      (let-values (((inp outp) (tcp-accept s)))
	(www-log "Accept: ~s ~%" s)
	(flush-output (current-output-port))
	(thread (λ ()
		   (call-with-exception-handler
		    (λ (e)
		       (displayln e)
		       ((error-display-handler) "Error in handling request." e)
		       (close-input-port inp)
		       (close-output-port outp))
		    (λ ()
		       (http-service-task dispatch-tree inp outp)))))
	(accept-loop)))))

(: http-service-task (DispatchTree Input-Port Output-Port -> Void))
(define (http-service-task dispatch-tree inp outp)
  (www-log  "Got in port ~s~%" in-port)
  (let ((request (read-request-header inp)))
    (www-log "Request ~s~%" request)
    (when request
      (dispatch request inp outp dispatch-tree))
    (close-input-port inp)
    (close-output-port outp)))

 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Main routine to launch the server. ;;
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(: webserver ((-> DispatchTree) Integer -> Void))
(define (webserver configure port)
  (let ((dispatch-tree (configure)))
    (www-log "Starting server on port ~s.~%" port)	
    (www-log "Dispatch Tree ~%")
    (displayln dispatch-tree)
    (call-in-nested-thread (λ () (http-server-task dispatch-tree port)))))

