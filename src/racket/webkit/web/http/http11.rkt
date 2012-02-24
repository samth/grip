;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Knozama's Amazon API Library
;; Copyright (C) 2007,2008,2009,2010,2011  Raymond Paul Racine
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;http-request

#lang typed/racket

(provide
 Action
 HTTPPayload HTTPPayload-mime HTTPPayload-md5
 http-action->string
 ResponseHeader
 ResponseHeader?
 ResponseHeader-result
 ResponseHeader-headers
 Result
 Result?
 Result-proto
 Result-code
 Result-msg
 HTTPConnection-in
 HTTPConnection-header
 http-successful?
 http-status-code
 http-has-content?
 http-close-connection
 http-invoke
 make-client-error-response
 parse-request-line
 parse-http-response-line)

;; (require/typed
;;  racket
;;  (read-bytes! (Bytes Input-Port Integer Integer -> Integer)))

(require/typed
 openssl/openssl (ssl-connect (String Integer -> (Values Input-Port Output-Port))))
 
(require/typed
 srfi/14
 (opaque char-set char-set?)
 (char-set:blank char-set)
 (char-set-complement (char-set -> char-set)))

(require/typed
 srfi/13
 (string-tokenize (String char-set -> (Listof String))))

(require/typed
 file/gunzip
 (gunzip-through-ports (Input-Port Output-Port -> Void)))

(require
 racket/date
 (only-in (planet knozama/common:1/std/control)
          aif)
 (only-in "proxy.rkt"
          http-proxy-port
          http-proxy-host
          http-proxy?)
 (only-in "../uri.rkt"
          Uri
          uri->start-line-path-string
	  Uri-scheme
          Authority-port
          Authority-host
          Uri-authority)
 (only-in "heading.rkt"
          HOST
          USER-AGENT)
 (only-in "header.rkt"
          make-header-string get-header get-header-value
          Header Headers))

(define-type Action (U 'GET 'PUT 'POST 'DELETE 'HEAD))

(struct: Result [(proto : String)
		 (code  : Integer)
		 (msg   : String)] #:transparent)

(struct: ResponseHeader [(result : Result)
			 (headers : (Listof Header))] #:transparent)

(struct: HTTPConnection ([header : ResponseHeader]
			 [out : Output-Port]
			 [in  : Input-Port]
			 [real-in : (Option Input-Port)])
	 #:transparent)  ;; the actual socket input port, e.g. 'in' could be a chuncking pipe 


(struct: HTTPPayload ([mime    : String]
		      [md5     : (Option String)]
		      [length  : (Option Index)]
		      [inport  : Input-Port]) #:transparent)

(: CHUNK-SIZE Exact-Nonnegative-Integer)
(define CHUNK-SIZE (* 64 1024)) ;; 64 K chunks, based on reasonable in-memory needs.

(: make-client-error-response (Integer String -> ResponseHeader))
(define (make-client-error-response code msg)
  (ResponseHeader (Result "HTTP/1.1" code msg) '()))

;; Do a substring, trimming spaces
(: substring-trim (String Integer Integer -> String))
(define (substring-trim src-str start end)
  (let ((s-pos (do: : Integer ((s-pos start (add1 s-pos)))
                 ((or (eqv? s-pos end)
                      (not (eqv? (string-ref src-str s-pos) #\space)))
                  s-pos)))
        (e-pos (do: : Integer  ((e-pos end (sub1 e-pos)))
                 ((or (eqv? e-pos start)
                      (not (eqv? (string-ref src-str e-pos) #\space)))
                  (if (< e-pos end)
                      (add1 e-pos)
                      e-pos)))))
    (substring src-str s-pos e-pos)))

;;-;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; THE HTTP REQUEST							      		       ;;
;; For now we avoid a structure and parse the http request header as follows:  	       ;;
;; HTTP Request :=  (cons start-line headers)				      		       ;;
;; start-line := "<METHOD> <PATH> HTTP/<VERSION>"			      		       ;;
;; headers := (alist (header . value)) 					      	       ;;
;; 											       ;;
;; The following routines support extracting information from the above data structure.       ;;
;;-;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Parses a HTTP start line into its three components		      ;;
;; 1) Method 2) URL path... 3) HTTP Version			      ;;
;; "GET /a/b/c/d.txt HTTP/V1.1" -> ("GET" "a/b/c/d.txt" "HTTP/V1.1") ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(: parse-request-line (String -> (Listof String)))
(define (parse-request-line sline)
  (string-tokenize sline (char-set-complement char-set:blank)))

(: request-line-method ((Listof String) -> String))
(define (request-line-method parsed-start-line)
  (car parsed-start-line))

(: request-line-path ((Listof String) -> String))
(define (request-line-path parsed-start-line)
  (cadr parsed-start-line))

(: request-line-version ((Listof String) -> String))
(define (request-line-version parsed-start-line)
  (caddr parsed-start-line))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Parse http response line
;; "HTTP/1.1 500 Internal Server Error"
;; -> ("HTTP/1.1" "500" "Internal Server Error")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(: parse-http-response-line (String -> (Option Result)))
(define (parse-http-response-line resp-line)
  (let ((len (string-length resp-line)))
    (if (<= len 12)
       #f
       (let ((proto (if (char=? (string-ref resp-line 8) #\space)
		     (substring resp-line 0 8)
		     #f))
	   (code (if (char=? (string-ref resp-line 12) #\space)
		    (let ((cd (string->number (substring resp-line 9 12))))
		      (if (exact-integer? cd)
			 cd
			 #f))
		    #f))
	   (msg (if (< 12 len)
		   (substring resp-line 13 len)
		   #f)))
	 (if (and proto code msg)
	    (Result proto code msg)
	    #f)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Chunked Encoding routines ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(: get-chunk-length (Input-Port -> Integer))
(define (get-chunk-length ip)
  
  (: char-hex? (Char -> Boolean))
  (define (char-hex? ch)
    (or (char-numeric? ch)
        (case ch
          ((#\a #\b #\c #\d #\e #\f) #t)
          ((#\A #\B #\C #\D #\E #\F) #t)
          (else #f))))
  
  (: read-chunk-length (Input-Port -> Integer))
  (define (read-chunk-length ip)
    (let ((osp (open-output-string)))
      (let ((ch (peek-char ip)))
        (if (eof-object? ch)
	   0
	   (when (eqv? ch #\return)
	     (read-char ip)     ;; return
	     (read-char ip))))  ;; linefeed
      (let: loop : Integer ((ch : (U Char EOF) (read-char ip)))
	  (cond 
	   ((eof-object? ch)         
	    0)
	   ((char=? ch #\space) ;; yahoo for one right pads chunk value with spaces.
	    (loop (read-char ip)))   ;; skip them.
	   ((char-hex? ch)
	    (begin
	      (write-char ch osp )
	      (loop (read-char ip))))
	   ((eqv? #\return ch)
	    (let ((ch (peek-char ip)))
	      (if (eof-object? ch)
                 0
                 (if (eqv? ch #\linefeed)
		    (begin (read-char ip)
		       (let ((sz (string->number (get-output-string osp) 16)))
			 (if (exact-integer? sz)
			    sz
			    0)))
		    0))))
	   (else 0)))))
  
  (with-handlers ([exn:fail?		    
		  (lambda (ex) 0)])
    (read-chunk-length ip)))

(: chunked-encoding? (Headers -> Boolean))
(define (chunked-encoding? headers)
  (let ((hdr (get-header-value "Transfer-Encoding" headers)))
    (if hdr
        (string-ci=? "chunked" hdr)
        #f)))

;; extract the http request Content-Length
;; return #f if Content-Length is not present
(: content-length (Headers -> (Option Integer)))
(define (content-length headers)
  (let ((len (get-header-value "Content-Length" headers)))
    (if len
       (assert (string->number len) exact-integer?)
       #f)))

;; returns:
;;   'chunked
;;   number? is content-length
;;   #f is bad and an error
(: content-length-or-chunked? (Headers -> (U False Symbol Integer)))
(define (content-length-or-chunked? headers)
  (let ((len (content-length headers)))
    (if len
        len
        (if (chunked-encoding? headers)
            'chunked
            #f))))

;; Read the http header by reading the given port until \r\n\r\n is found.
;; port? -> (cons start-line headers)
;; start-line := "<METHOD> <PATH> HTTP/<VERSION>"
;; Why not just a readline?
;;  - Well in R6RS once I switch a port to text I can't go back to binary and the payload
;;    may very well be a binary one.
;;  - So the header read was written as byte by byte processing.
;;  - With Racket there are no binary vs text ports.
;;  -  so can now do simple (read-line ip 'return-linefeed).  One day ...

;; (define-type Header (Pair String String))
	 
(define-type Rev-HTTP-Resp-Header (Rec Rev-HTTP-Resp-Header 
				  (U (Pair Header Rev-HTTP-Resp-Header) 
				     (List String) 
				     Null)))

(define-type HTTP-Resp-Header (Pair String (Listof Header)))

(require/typed racket
               ((cons resp-header-cons) (Header Rev-HTTP-Resp-Header -> Rev-HTTP-Resp-Header))
               ((cons resp-msg-cons)    (String Rev-HTTP-Resp-Header -> Rev-HTTP-Resp-Header))
               ((reverse reverse-response) (Rev-HTTP-Resp-Header -> HTTP-Resp-Header)))

;; Max size allowed for an HTTP response
(define MAX-REQUEST (* 3 1024))

(: make-response-header-from-parsed (Rev-HTTP-Resp-Header -> (Option ResponseHeader)))
(define (make-response-header-from-parsed rev-header)
  (let ((results (reverse-response rev-header)))
    (let ((result-str (car results)))
      (if (string? result-str)		  
	 (let ((result (parse-http-response-line result-str)))
	   (if result
	      (ResponseHeader result (cdr results))
	      #f))
	 #f))))
	 

(: http-header-from-socket-input-port (Input-Port -> (Option ResponseHeader)))
(define (http-header-from-socket-input-port inp)
  (let ((req (make-string MAX-REQUEST)))
    (let: loop : (Option ResponseHeader)
        ((state : Integer 0) 
         (cnt : Integer 0) 
         (byte : (U EOF Byte) (read-byte inp)) 
         (caret : Integer 0) 
         (colon : Integer 0) 
         (headers : Rev-HTTP-Resp-Header '()))      
        (if (eqv? cnt MAX-REQUEST)
	   #f                                                           ;; FIXME return 4XX
	   (if (eof-object? byte)
	      (make-response-header-from-parsed headers)
	      (let ((state (case state
			   ((0) (case byte
				  ((#x0D) 1)
				  (else  0)))
			   ((1) (case byte
				  ((#x0A) 2)
				  (else  0)))
			   ((2) (case byte
				  ((#x0D) 3)
				  (else 0)))
			   ((3) (case byte
				  ((#x0A) 4)
				  (else 0)))
			   (else 0))))
		(case state
		  ((2) (let ((ch (integer->char byte)))
			 (string-set! req cnt ch)
			 (loop state
			       (add1 cnt)
			       (read-byte inp)
			       (add1 cnt)
			       -1                      ;; colon <> -1 mean we found the first one already.  ':' is a legitimate header value, only first ':" is a delim.
			       (if (zero? colon)       ;; HTTP line as no colon was found
				  (resp-msg-cons (substring req caret (sub1 cnt)) headers)
				  (resp-header-cons (cons (substring req caret colon) 
							  (substring-trim req (add1 colon) (sub1 cnt))) ;; header line (attr . value)
						    headers)))))
		  ((4) (make-response-header-from-parsed headers))
		  (else
		   (let ((ch (integer->char byte)))
		     (string-set! req cnt ch)
		     (loop state
			   (add1 cnt)
			   (read-byte inp)
			   caret
			   (if (and (eqv? colon -1)
				 (eqv? ch #\:))
			      cnt                ;; found a colon at position cnt
			      colon)
			   headers))))))))))


(: space String)
(define space " ")
(: version String)
(define version "HTTP/1.1")
(: terminate String)
(define terminate "\r\n")

(: failed-connection (String -> HTTPConnection))
(define failed-connection
  (let ((in   (open-input-string ""))
      (out  (open-output-string ""))
      (base-msg "Bad Request - "))
    (close-input-port in)
    (close-output-port out)
    (lambda (msg)
	(HTTPConnection (ResponseHeader (Result "HTTP/1.1" 400 (string-append base-msg msg)) 
					'())
			out in #f))))

;; Used by the chunk reader thread to pipe the chunks.
;; Intermediate pipe for inbound chunked data stream
;; Read off the chunk size from the input port
;; then transfer that amount of data from the input 
;; to the output port.
(: http-pipe-chunks (Integer Input-Port Output-Port -> Void))
(define (http-pipe-chunks chunk-size socket-ip out-pipe)
  (let: loop : Void ((chunk-size : Integer chunk-size))
      (if (zero? chunk-size)
	 (begin
	   (flush-output out-pipe)
	   (close-output-port out-pipe))
	 (let ((bs (read-bytes chunk-size socket-ip)))
	   (if (eof-object? bs)
	      (begin
		(flush-output out-pipe)
		(close-output-port out-pipe))
	      (begin (write-bytes bs out-pipe)
		 (loop (get-chunk-length socket-ip))))))))

(: http-pipe-content-length-block (Integer Input-Port Output-Port -> Void))
(define (http-pipe-content-length-block content-length socket-ip out-pipe)
  
  (define (complete)
    (flush-output out-pipe)
    (close-output-port out-pipe))

  (let ((bs (read-bytes content-length socket-ip)))
    (if (eof-object? bs)
       (complete)
       (begin
	 (write-bytes bs out-pipe)
	 (complete)))))

(: http-pipe-gunzip (Input-Port Output-Port -> Void))
(define (http-pipe-gunzip inp outp)
  (gunzip-through-ports inp outp))

  ;; (define buffsz (* 16 1024))
  ;; (define (complete)
  ;;   (flush-output outp)
  ;;   (close-output-port outp))
  


  ;; (let: loop : Void ((bs : (U EOF Bytes) (read-bytes buffsz inp)))
  ;;     (if (eof-object? bs)
  ;; 	 (complete)
  ;; 	 (begin
  ;; 	   (write-bytes bs outp)
  ;; 	   (loop (read-bytes buffsz inp))))))

(: http-action->string (Action -> String))
(define (http-action->string action)
  (case action
    ((GET)    "GET")
    ((PUT)    "PUT")
    ((POST)   "POST")
    ((DELETE) "DELETE")
    ((HEAD)   "HEAD")))

(: send-header (String String Output-Port -> Void))
(define (send-header header value op)
  (write-string header op)
  (write-string ": " op)
  (write-string value op)
  (write-string terminate op)
  (void))

;; Write out the HTTP header line and accompaning headers converted to strings.
;; NOTE the headers are NOT terminated which is done by send-payload as
;; a content-length may be required if not chunked.
(: send-http-header (Output-Port Action Uri (Listof String) -> Void))
(define (send-http-header op action url headers)
  (write-string (http-action->string action) op)
  (write-string space op)
  (write-string (uri->start-line-path-string url) op)
  (write-string space op)
  (write-string version op)
  (write-string terminate op)
  (for-each (lambda: ((h : String))
	      (write-string h op)
	      (write-string terminate op))
	    headers))

(: terminate-http-header (Output-Port -> Void))
(define (terminate-http-header op)
  (write-string terminate op)
  (flush-output op))

(: send-chunked-payload (Input-Port Output-Port -> Void))
(define (send-chunked-payload ip op)

  (: write-chunk-header (Integer -> Void))
  (define (write-chunk-header sz)
    (write-string (number->string sz 16) op)
    (write-string terminate op)
    (void))

  (let ((buffer (make-bytes CHUNK-SIZE)))
    (let loop ((sz (read-bytes! buffer ip 0 CHUNK-SIZE)))      
      (if (eof-object? sz)
	 (begin
	   (write-chunk-header 0)
	   (write-string terminate op)
	   (void))
	 (begin
	   (write-chunk-header sz)
	   (write-bytes buffer op 0 (- sz 1))
	   (loop (read-bytes! buffer ip 0 CHUNK-SIZE)))))))

(: send-contentlength-payload (HTTPPayload Output-Port -> Void))
(define (send-contentlength-payload payload outp)
  (let ((inp (HTTPPayload-inport payload))
	(length (assert (HTTPPayload-length payload))))
    (send-header "Content-Length" (number->string length 10) outp)
    (send-header "Content-Type" (HTTPPayload-mime payload) outp)
    (let ((md5 (HTTPPayload-md5 payload)))
      (when md5
	(send-header "Content-MD5" md5 outp)))
    (terminate-http-header outp)
    (let* ((buff-sz (if (< length CHUNK-SIZE)
			length
			CHUNK-SIZE))
	   (buffer (make-bytes buff-sz)))
      (do ([sz (read-bytes! buffer inp 0 buff-sz)
	       (read-bytes! buffer inp 0 buff-sz)])
	  ((eof-object? sz) (flush-output outp))
	(write-bytes buffer outp)))))

(: send-payload (HTTPPayload Output-Port -> Void))
(define (send-payload payload op)
  (let ((len (HTTPPayload-length payload))
	(inport (HTTPPayload-inport payload)))
    (if len
	(send-contentlength-payload payload op)
	(begin
	  (send-header "Transfer-Encoding" "Chunked" op)
	  (terminate-http-header op)
	  (send-chunked-payload inport op)))
     (terminate-http-header op)))
  
;; WARNING - All the output routines now in Racket (write-bytes etc) return the actual
;; number of bytes written.  All the above code "assumes" a full write always occurs to 
;; the socket output port. FIXME 

;; Put - If the payload is a byte array then used content-length.
;;     - If the payload is a pipe, use chunking.
;; ... except that approach won't work for S3 for example which does not support
;;     chunking, yet we want to say stream a large file and its length is available
;;     via an O/S system call.
;; Use an explicit Payload structure with an optional length so a ports length 
;; can be explicitly given, wherein content-length will be used.
(: http-invoke (Action Uri (Listof String) (Option HTTPPayload) -> HTTPConnection))
(define (http-invoke action url headers payload)

  (: append-host-to-headers (String -> (Listof String)))
  (define (append-host-to-headers host)
    (cons (make-header-string HOST host) headers))
  
  (let ((authority (Uri-authority url)))
    (if (not  authority)
       (failed-connection "Missing authority in URL")
        (let* ((host (Authority-host authority))
	     (headers (append-host-to-headers host))
	     (port    (aif (Authority-port authority) it 80))
	     (proxy? (http-proxy? authority url)))
          (let ((conn-host (if proxy?
			    (aif (http-proxy-host) it host)
			    host))
	      (conn-port (if proxy?
			    (aif (http-proxy-port) it port)
			    port)))
            (let-values (((ip op) (if (string=? (Uri-scheme url) "https")
				   (ssl-connect conn-host conn-port)
				   (tcp-connect conn-host conn-port))))
	      (send-http-header op action url headers)
	      
	      (if payload
		  (send-payload payload op)
		  (terminate-http-header op))
	      
	      (let ((headers (http-header-from-socket-input-port ip)))
		(if headers
		   (let  ((chunked/length (content-length-or-chunked? (ResponseHeader-headers headers)))
			(encoding (get-header-value "Content-Encoding" (ResponseHeader-headers headers))))
		     (if (number? chunked/length)
			;; content/length
			(let-values (((inpipe outpipe) (make-pipe)))
			  (thread (lambda () (http-pipe-content-length-block chunked/length ip outpipe)))
			  (if (and encoding (string=? "gzip" encoding))
			     (let-values (((gz-inp gz-outp) (make-pipe)))
			       (thread (lambda () (http-pipe-gunzip inpipe gz-outp)))
			       (HTTPConnection headers op gz-inp ip))
			     (HTTPConnection headers op inpipe ip)))
			;; chunked
			(let-values (((inpipe outpipe) (make-pipe)))
			  (thread (lambda () (http-pipe-chunks (get-chunk-length ip) ip outpipe)))
			  (if (and encoding (string=? "gzip" encoding))
			     (let-values (((gz-inp gz-outp) (make-pipe)))
			       (thread (lambda () (http-pipe-gunzip inpipe gz-outp)))
			       (HTTPConnection headers op gz-inp ip))
			     (HTTPConnection headers op inpipe ip)))))
		   (failed-connection "Invalid response from server")))))))))


(: http-status-code (HTTPConnection -> Integer))
(define (http-status-code conn)
  (Result-code (ResponseHeader-result (HTTPConnection-header conn))))
 
(: http-successful? (HTTPConnection -> Boolean))
(define (http-successful? conn)
  (eq? (Result-code (ResponseHeader-result (HTTPConnection-header conn))) 200))

(: http-has-content? (HTTPConnection -> Boolean))
(define (http-has-content? connection)
  (let ((content (content-length-or-chunked? 
		  (ResponseHeader-headers (HTTPConnection-header connection)))))
    (cond 
     ((exact-integer? content) (> content 0))
     ((symbol? content) #t)
     (else #f))))

(: http-close-connection (HTTPConnection -> Void))
(define (http-close-connection conn)
  (let ((real-in (HTTPConnection-real-in conn)))
    (if real-in
       (begin
	 (close-input-port (HTTPConnection-in conn))
	 (close-input-port real-in))
       (close-input-port (HTTPConnection-in conn)))
    (close-output-port (HTTPConnection-out conn))))
