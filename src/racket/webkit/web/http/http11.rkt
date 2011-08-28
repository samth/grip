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
 HTTP-Resp-Header
 http-invoke
 http-response-from-headers
 response-line-code)

;; (provide http-header-from-socket-input-port
;;          get-header header-value
;;          content-length-or-chunked?
;; 	 http-response-from-headers
;; 	 parse-http-response-line
;;          response-line-code
;;          response-line-msg
;;          http-send-response
;;          http-301-moved-permanently
;; 	 parse-request-line
;;          request-line-method 
;;          request-line-path 
;;          request-line-version)


(require/typed
 racket
 (read-bytes! (Bytes Input-Port Integer Integer -> Integer)))

(require/typed
 srfi/14
 (opaque char-set char-set?)
 (char-set:blank char-set)
 (char-set-complement (char-set -> char-set)))

(require/typed
 srfi/13
 (string-tokenize (String char-set -> (Listof String))))

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
	  Authority-port
	  Authority-host
	  Uri-authority)
 (only-in "heading.rkt"
	  HOST
	  USER-AGENT)
 (only-in "header.rkt"
	  make-header-string
	  Header
	  Headers))


;; (require racket/date
;; 	 racket/tcp
;;          (only-in srfi/13
;;                   string-tokenize)
;;          (only-in typed/srfi/14
;;                   char-set-complement
;;                   char-set:blank
;;                   char-set:whitespace
;;                   char-set:graphic
;;                   char-set-contains?)
;;          "../uri.rkt"
;;          "proxy.rkt")

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
(: parse-http-response-line (String -> (Listof String)))
(define parse-http-response-line
  (lambda (resp-line)
    (let ((len (string-length resp-line)))
      (if (<= len 12)
	 (list "" "" "")
	 (list 
	  (if (char=? (string-ref resp-line 8) #\space)
	     (substring resp-line 0 8)
	     "")
	  (if (char=? (string-ref resp-line 12) #\space)
	     (substring resp-line 9 12)
	     "")
	  (if (< 12 len)
	     (substring resp-line 13 len)
	     ""))))))

(: response-line-code ((Option (Listof String)) -> (Option String)))
(define (response-line-code response-line)
  (if response-line
     (cadr response-line)
     #f))
	   
(: response-line-msg ((Listof String) -> String))
(define response-line-msg  caddr)

(: http-response-from-headers (HTTP-Resp-Header -> (Option (Listof String))))
(define (http-response-from-headers headers)
  (if (null? headers)
     #f
     (parse-http-response-line (car headers))))


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
	 (else #f))))
  
  (: read-chunk-length (Input-Port (Input-Port -> (U Char EOF)) (Input-Port -> (U Char EOF)) -> Integer))
  (define (read-chunk-length ip next peek)    
    (let ((osp (open-output-string)))
      (let ((ch (peek ip)))
	(if (eof-object? ch)
	   0
	   (when (eqv? ch #\return)
	     (next ip)     ;; return
	     (next ip))))  ;; linefeed
      (let: loop : Integer ((ch : (U Char EOF) (next ip)))
	  (cond 
	   ((eof-object? ch)         
	    0)
	   ((char=? ch #\space) ;; yahoo for one right pads chunk value with spaces.
	    (loop (next ip)))   ;; skip them.
	   ((char-hex? ch)
	    (begin
	      (write-char ch osp )
	      (loop (next ip))))
	   ((eqv? #\return ch)
	    (let ((ch (peek ip)))
	      (if (eof-object? ch)
		 0
		 (if (eqv? ch #\linefeed)
		    (begin (next ip)
		       (assert (string->number (get-output-string osp) 16) exact-integer?))
		    0))))
	   (else 0)))))
  
  
  (read-chunk-length ip read-char peek-char))


(: get-header (String Headers -> (Option Header)))
(define get-header
  (lambda (attr headers)
    (assoc attr headers)))

(: header-value ((Option Header) -> (Option String)))
(define (header-value hdr)
  (if (pair? hdr)
     (cdr hdr)
     #f))

(: chunked-encoding? (Headers -> Boolean))
(define (chunked-encoding? headers)
  (let ((hdr (get-header "Transfer-Encoding" headers)))
    (if hdr
       (string-ci=? "chunked" (cdr hdr))
       #f)))

;; extract the http request Content-Length
;; return #f if Content-Length is not present
(: content-length (Headers -> (Option Integer)))
(define (content-length headers)
  (let ((len (header-value (get-header "Content-Length" headers))))
    (if (string? len)
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
(define-type HTTP-Resp-Header (Pair String (Listof Header)))
(define-type Rev-HTTP-Resp-Header (Rec Rev-HTTP-Resp-Header 
				  (U (Pair Header Rev-HTTP-Resp-Header) 
				     (List String) 
				     Null)))

(require/typed racket
	       ((cons resp-header-cons) (Header Rev-HTTP-Resp-Header -> Rev-HTTP-Resp-Header))
	       ((cons resp-msg-cons)    (String Rev-HTTP-Resp-Header -> Rev-HTTP-Resp-Header))
	       ((reverse reverse-response) (Rev-HTTP-Resp-Header -> HTTP-Resp-Header)))

(: http-header-from-socket-input-port (Input-Port -> (Option HTTP-Resp-Header)))
(define (http-header-from-socket-input-port inp)

  (let ((MAX-REQUEST 1024))
    (let ((req (make-string MAX-REQUEST)))
      (let: loop : (Option HTTP-Resp-Header)
	  ((state : Integer 0) 
	   (cnt : Integer 0) 
	   (byte : (U EOF Byte) (read-byte inp)) 
	   (caret : Integer 0) 
	   (colon : Integer 0) 
	   (headers : Rev-HTTP-Resp-Header '()))
	  (if (eqv? cnt MAX-REQUEST)
	     #f                                                           ;; FIXME return 4XX
	     (if (eof-object? byte)
		(reverse-response headers)
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
				    (resp-header-cons (cons (substring req caret colon) (substring-trim req (add1 colon) (sub1 cnt))) ;; header line (attr . value)
						      headers)))))
		    ((4) (reverse-response headers))
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
			     headers)))))))))))


(: space String)
(define space " ")
(: version String)
(define version "HTTP/1.1")
(: terminate String)
(define terminate "\r\n")

(define failed-input-port (open-input-string ""))
(define failed-invoke-resp (reverse-response (resp-header-cons (cons USER-AGENT  "SOS/RL3/0.1")
							  (list "HTTP/1.1 400 Bad Request - Invalid URI?"))))

;; ;; Used by the chunk reader thread to pipe the chunks.
(: http-pipe-chunks (Integer Input-Port Output-Port -> Void))
(define (http-pipe-chunks chunk-size socket-ip out-pipe)
  (let: loop : Void ((chunk-size : Integer chunk-size))
      (if (zero? chunk-size)
	 (begin (close-input-port socket-ip) 
	    (flush-output out-pipe)
	    (close-output-port out-pipe))
	 (let ((bs (read-bytes chunk-size socket-ip)))
	   (if (eof-object? bs)
	      (loop 0)
	      (begin (write-bytes bs out-pipe)
		 (loop (get-chunk-length socket-ip))))))))

;; ;; (provide/contract (http-invoke (-> symbol? uri? any/c (or/c boolean? bytes?) any)))

(: http-invoke (Symbol Uri (Listof String) (Option Bytes) -> (values HTTP-Resp-Header Input-Port)))
(define (http-invoke action url headers payload)
  (let ((authority (Uri-authority url)))
    (if (not  authority)
       (values failed-invoke-resp failed-input-port)
       (let* ((host (Authority-host authority))
	    (headers (cons (make-header-string HOST host) headers))
	    (port (let ((port (Authority-port authority)))
		    (if port port 80)))
	    (verb (lambda (action)
		    (case action
		      ((GET) "GET")
		      ((PUT) "PUT")
		      ;;  ((POST) (string->utf8 "POST"))
		      ;;  ((HEAD) (string->utf8 "HEAD")))))
		      (else (error 'http-invoke "HTTP method not support." action)))))
	    (proxy? (http-proxy? authority url)))
	 (let ((conn-host (if proxy?
			   (aif (http-proxy-host) it host)
			   host))
	     (conn-port (if proxy?
			   (aif (http-proxy-port) it port)
			   port)))
	   (let-values (((ip op) (tcp-connect conn-host conn-port)))
	     (let ((send (lambda: ((s : String))
			 (write-string s op))))
	       ;; header line
	       (send (verb action))
	       (send space)
	       (send (uri->start-line-path-string url))
	       (send space)
	       (send version)
	       (send terminate)
	       ;; headers
	       (for-each (lambda: ((h : String))
			   (send h)
			   (send terminate))
			 headers)
	       (when payload
		 (send (string-append "Content-Length: " 
				      (number->string (bytes-length payload))))
		 (send terminate))
	       (send terminate)
	       (when payload
		 ;; (send payload)  FIXME RPP
		 (send terminate))
	       (flush-output op)             

	       (close-output-port op) ;; nginx doesn't understand a half-close <sigh>
	       (let ((headers (http-header-from-socket-input-port ip)))
		 (if headers
		    (let  ((chunked/length (content-length-or-chunked? (cdr headers))))
		      (if (number? chunked/length)
			 (values headers ip) ;; content/length
			 (let-values (((inpipe outpipe) (make-pipe)))
			   (thread (lambda () (http-pipe-chunks (get-chunk-length ip) ip outpipe)))
			   (values headers inpipe))))
		    (values failed-invoke-resp failed-input-port))))))))))

;; (: current-time-rfc2822 (-> String))
;; (define (current-time-rfc2822)
;;   (date-display-format 'rfc2822)
;;   (date->string (seconds->date (current-seconds))))

;; (: http-send-response (String (Listof (Pair String String)) Output-Port Input-Port Integer -> Void))
;; (define (http-send-response code-str headers socket-output-port content-input-port length)
;;   (let ((preamble   "HTTP/1.1 ")
;;       (code       code-str)
;;       (colon-sp    ": ")
;;       (send (lambda: ((str : String))
;; 	      (write-string str socket-output-port))))
;;     (let ((send-header
;; 	 (lambda (hdr)
;; 	   (send (car hdr))
;; 	   (send colon-sp)
;; 	   (send (cdr hdr))
;; 	   (send terminate))))
;;       (send preamble)
;;       (send code)
;;       (send terminate)
;;       (send-header (cons "Date" (current-time-rfc2822)))
;;       (for-each send-header headers)
;;       (if (and (input-port? content-input-port) (zero? length))
;; 	 (send-header (cons "Transfer-Encoding" "Chunked"))	   
;; 	 (send-header (cons "Content-Length" (number->string length 16))))
;;       (send terminate)
;;       (if (input-port? content-input-port)
;; 	 (let ((buffsz 1024))
;; 	   (let ((buffer (make-bytes buffsz)))
;; 	     (let:  loop : Void ((cnt : Integer (read-bytes! buffer content-input-port 0 buffsz)))
;; 	       (if (eof-object? cnt)
;; 		  (begin
;; 		    (send "0")
;; 		    (send terminate)(send terminate))
;; 		  (begin
;; 		    (send (number->string cnt 16))
;; 		    (send terminate)
;; 		    (write-bytes buffer socket-output-port 0 cnt)
;; 		    (send terminate)
;; 		    (loop  (read-bytes! buffer content-input-port 0 buffsz)))))))
;; 	 (send terminate)))))


;; (: http-301-moved-permanently (String Output-Port -> Void))
;; (define (http-301-moved-permanently loc socket-output-port)
;;   (let ((headers (list (cons "Location"  loc)
;; 		     (cons "Content-Type" "text/html")))
;;       (redir-msg (string-append "Please follow <a href=\"" 
;; 				loc 
;; 				"\">Knozama</a>")))
;;     (http-send-response "301 moved permanently" 
;; 			headers
;; 			socket-output-port
;; 			(open-input-bytes (string->bytes/utf-8 redir-msg))
;; 			0)))

;; (cond
;;  ((textual-port? ip)
;;   (read-chunk-length ip get-char lookahead-char))
;;  ((binary-port? ip)
;;   (read-chunk-length ip get-char-binary-port lookahead-char-binary-port)))))

;; Assumes the port http headers have been read.
;; Extra buffer copy here as I'm wrapping a port within a port
;; Create a binary input port which understands the http protocol.
;; e.g., seamless handling of chunked encoding.
;; Ideally I'd like to avoid double buffer filling
;; but the R6RS machinary and maybe Larceny's don't make this
;; readily achievable. Really need a Streams/Port approach such
;; as proposed in SRFI-81,82,83.

;; If content-length is false then assume chunked encoding.
;; input-port? * number? -> binary-input-port?

;; content-length/chunked?
;;   #f - header has not been read
;;   'chunked - chunked
;;   number? - content-length
;; id - name of the port??
;; ip - socket input port
;; (define make-http-binary-input-port
;;   (lambda (id ip content-length/chunked?)

;;     (define name (string-copy id))

;;     (define chunked?
;;       (eq? 'chunked content-length/chunked?))

;;     (define left-to-read 
;;       (cond
;;        ((number? content-length/chunked?)
;; 	content-length/chunked?)
;;        (chunked? (get-chunk-length ip))
;;        (error 'make-http-binary-input-port "HTTP Protocol must have content-length or chunked encoding.")))

;;     ;; assumed only called if there are at least some bytes available
;;     (define read-proc
;;       (lambda (iodata buffer)
;; 	(if (fxzero? left-to-read)
;; 	   'eof
;; 	   (let ((buffsz (bytes-length buffer)))
;; 	     (let ((cnt (read-bytes-available! ip buffer 0                 
;; 					     (if (fx<? left-to-read buffsz)
;; 						left-to-read
;; 						buffsz))))
;; 	       (set! left-to-read (fx- left-to-read cnt))
;; 	       (if (fxzero? cnt)                    ;; socket peer reset
;; 		  'eof
;; 		  (when (and chunked?
;; 			   (fxzero? left-to-read))
;; 		    (set! left-to-read (get-chunk-length ip))))
;; 	       cnt)))))

;;     (define iproc
;;       (lambda (op)
;; 	(case op
;; 	  ((read) read-proc)
;; 	  ((close) (lambda (io-data) (close-port ip)))
;; 	  ((name)  (lambda (io-data) name))
;; 	  (else (assertion-violation 'make-http-binary-input-port "unsupported IO operation" id op)))))

;;     (io/make-port iproc name 'input 'binary)))
