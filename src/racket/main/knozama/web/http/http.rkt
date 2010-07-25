;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Knozama's Amazon API Library
;; Copyright (C) 2007,2008,2009,2010  Raymond Paul Racine
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

#lang racket

(provide http-header-from-socket-input-port
         ;; http-invoke
         ;; open-http-binary-input-port
         ;;make-http-binary-input-port
         get-header header-value
         content-length-or-chunked?
         parse-http-header
         parse-http-response-line
         response-line-code
         response-line-msg
         http-send-response
         http-301-moved-permanently
         http-header-method 
         http-header-path 
         http-header-version)

(provide/contract (http-invoke (-> symbol? uri? any/c (or/c boolean? bytes?) any)))

(require racket/fixnum
         racket/date
         (only-in knozama/std/control
                  aif)
         knozama/web/uri
         knozama/web/http/proxy
         (only-in knozama/std/prelude
                  fxzero? fx1+ fx1-)
         (only-in srfi/13
                  string-tokenize)
         (only-in srfi/14
                  char-set-complement
                  char-set:blank
                  char-set:whitespace
                  char-set:graphic
                  char-set-contains?))


;; Do a substring, trimming spaces
(define substring-trim
  (lambda (src-str start end)
    (let ((s-pos (do ((s-pos start (fx1+ s-pos)))
                   ((or (fx= s-pos end)
                        (not (eqv? (string-ref src-str s-pos) #\space)))
                    s-pos)))
          (e-pos (do ((e-pos end (fx1- e-pos)))
                   ((or (fx= e-pos start)
                        (not (eqv? (string-ref src-str e-pos) #\space)))
                    (if (fx< e-pos end)
                        (fx1+ e-pos)
                        e-pos)))))
      (substring src-str s-pos e-pos))))

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

(define parse-http-header
  (lambda (sline)
    (string-tokenize sline (char-set-complement char-set:blank))))

(define http-header-method
  (lambda (parsed-start-line)
    (car parsed-start-line)))

(define http-header-path
  (lambda (parsed-start-line)
    (cadr parsed-start-line)))

(define http-header-version
  (lambda (parsed-start-line)
    (caddr parsed-start-line)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Parse http response line
;; "HTTP/1.1 500 Internal Server Error"
;; -> ("HTTP/1.1" "500" "Internal Server Error"
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define parse-http-response-line
  (lambda (resp-line)
    (let ((len (string-length resp-line)))
      (if (fx<= len 12)
          (list "" "" "")
          (list 
           (if (char=? (string-ref resp-line 8) #\space)
               (substring resp-line 0 8)
               "")
           (if (char=? (string-ref resp-line 12) #\space)
               (substring resp-line 9 12)
               "")
           (if (fx< 12 len)
               (substring resp-line 13 len)
               ""))))))

(define response-line-code cadr)
(define response-line-msg  caddr)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Chunked Encoding routines ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define get-chunk-length   
  (lambda (ip)
    
    (define char-hex?
      (lambda (ch)
        (or (char-numeric? ch)
            (case ch
              ((#\a #\b #\c #\d #\e #\f) #t)
              (else #f)))))
    
    (define get-char-binary-port
      (lambda (ip)
        (let ((u8 (read-byte ip)))
          (if (eof-object? u8)
              u8
              (integer->char u8)))))
    
    (define lookahead-char-binary-port
      (lambda (ip)
        (let ((u8 (peek-byte ip)))
          (if (eof-object? u8)
              u8
              (integer->char u8)))))
    
    (define read-chunk-length
      (lambda (ip next peek)
        (let ((osp (open-output-string)))
          (let ((ch (peek ip)))
            (if (eof-object? ch)
                0
                (when (eqv? ch #\return)
                  (next ip)     ;; return
                  (next ip))))  ;; linefeed
          (let loop ((ch (next ip)))
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
                         (begin
                           (next ip)
                           (string->number (get-output-string osp) 16))
                         0))))
              (else 0))))))
    
    
    (read-chunk-length ip read-char peek-char)))

(define get-header
  (lambda (attr headers)
    (assoc attr headers)))

(define header-value
  (lambda (hdr)
    (if (pair? hdr)
        (cdr hdr)
        #f)))

(define chunked-encoding?
  (lambda (headers)
    (let ((hdr (get-header "Transfer-Encoding" headers)))
      (if hdr
          (string-ci=? "chunked" (cdr hdr))
          #f))))

;; extract the http request Content-Length
;; return #f if Content-Length is not present
(define content-length
  (lambda (headers)
    (let ((len (header-value (get-header "Content-Length" headers))))
      (if (string? len)
          (string->number len)
          #f))))

;; returns:
;;   'chunked
;;   number? is content-length
;;   #f is bad and an error
(define content-length-or-chunked?
  (lambda (headers)
    (let ((len (content-length headers)))
      (if len
          len
          (if (chunked-encoding? headers)
              'chunked
              #f)))))

;; Read the http header by reading the given port until \r\n\r\n is found.
;; port? -> (cons start-line headers)
;; start-line := "<METHOD> <PATH> HTTP/<VERSION>"
;; Why not just a readline?
;;  - Well in R6RS once I switch a port to text I can't go back to binary and the payload
;;    may very well be a binary one.
;;  - So the header read was written as byte by byte processing.
;;  - With Racket there are no binary vs text ports.
;;  -  so can now do simple (read-line ip 'return-linefeed).  One day ...
(define http-header-from-socket-input-port
  (lambda (inp)
    (let ((MAX-REQUEST 1024))
      (let ((req (make-string MAX-REQUEST)))
        (let loop ((state 0) (cnt 0) (byte (read-byte inp)) (caret 0) (colon 0) (headers '()))
          (if (fx= cnt MAX-REQUEST)
              #f                                                           ;; FIXME return 4XX
              (if (eof-object? byte)
                  (reverse headers)
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
                                        (else 0))))))
                    (case state
                      ((2) (let ((ch (integer->char byte)))
                             (string-set! req cnt ch)
                             (loop state
                                   (fx1+ cnt)
                                   (read-byte inp)
                                   (fx1+ cnt)
                                   -1                      ;; colon <> -1 mean we found the first one already.  ':' is a legitimate header value, only first ':" is a delim.
                                   (if (fxzero? colon)       ;; HTTP line as no colon was found
                                       (cons (substring req caret (fx1- cnt)) headers)
                                       (cons (cons (substring req caret colon) (substring-trim req (fx1+ colon) (fx1- cnt))) ;; header line (attr . value)
                                             headers)))))
                      ((4) (reverse headers))
                      (else
                       (let ((ch (integer->char byte)))
                         (string-set! req cnt ch)
                         (loop state
                               (fx1+ cnt)
                               (read-byte inp)
                               caret
                               (if (and (fx= colon -1)
                                        (eqv? ch #\:))
                                   cnt                ;; found a colon at position cnt
                                   colon)
                               headers))))))))))))

(define space (string->bytes/utf-8 " "))
(define version (string->bytes/utf-8  "HTTP/1.1"))
(define terminate (string->bytes/utf-8 "\r\n"))

;; Used by the chunk reader thread to pipe the chunks.
(define http-pipe-chunks
  (lambda (chunk-size socket-ip out-pipe)
    (let loop ((chunk-size chunk-size))
      (display (format "Chunk size: ~s" chunk-size))
      (if (zero? chunk-size)
          (begin (flush-output out-pipe)
	     (close-output-port out-pipe))
          (begin
            (write-bytes (read-bytes chunk-size socket-ip) out-pipe)
            (loop (get-chunk-length socket-ip)))))))

(define http-invoke
  (lambda (action url headers payload)
    (let ((authority (uri-authority url)))
      (let ((host (authority-host authority))
            (port (let ((port (authority-port authority)))
                    (if port (string->number port) 80)))		    
            (verb (lambda (action)
                    ;; this is wrong string->ascii??
                    (case action
                      ((GET)  (string->bytes/utf-8 "GET"))
                      ((PUT)  (string->bytes/utf-8 "PUT"))
                      ;;  ((POST) (string->utf8 "POST"))
                      ;;  ((HEAD) (string->utf8 "HEAD")))))
                      (else (error 'http-invoke "HTTP method not support." action)))))
            (proxy? (http-proxy? authority url)))	 
        (let ((conn-host (if proxy?
                             host
                             (aif (http-proxy-host) it host)))
              (conn-port (if proxy?
                             port
                             (aif (http-proxy-port) it port))))
          (let-values (((ip op) (tcp-connect conn-host conn-port)))
            (let ((send (lambda (s)
                          (write-bytes s op))))
              ;; header line
              (send (verb action))
              (send space)
              (send (string->bytes/utf-8 (uri->start-line-path-string url))) 
              (send space)
              (send version)
              (send terminate)
              ;; headers
              (for-each (lambda (h)
                          (send (string->bytes/utf-8 h))
                          (send terminate))
                        headers)
              (when payload
                (send (string->bytes/utf-8 (string-append "Content-Length: " 
                                                          (number->string (bytes-length payload)))))
                (send terminate))
              (send terminate)
              (when payload
                (send payload)
                (send terminate))
              (flush-output op)             
              
              ;;(close-port op) ;; nginx doesn't understand a half-close <sigh>
              (let ((headers (http-header-from-socket-input-port ip)))
                (let  ((chunked/length (content-length-or-chunked? (cdr headers))))
                  (if (number? chunked/length)
                      (values headers ip) ;; content/length
                      (let-values (((inpipe outpipe) (make-pipe)))
                        (thread (lambda () (http-pipe-chunks (get-chunk-length ip) ip outpipe)))
                        (values headers inpipe))))))))))))


(define current-time-rfc2822
  (lambda ()
    (date-display-format 'rfc2822)
    (date->string (seconds->date (current-seconds)))))


(define http-send-response
  (lambda (code-str headers socket-output-port content-input-port length)
    (let ((preamble   "HTTP/1.1 ")
          (code       code-str)
          (colon-sp    ": ")
          (send (lambda (bstr) 
                  (write-string socket-output-port bstr))))
      (let ((send-header
             (lambda (hdr)
               (send (car hdr))
               (send colon-sp)
               (send (cdr hdr))
               (send terminate))))
        (send preamble)
        (send code)
        (send terminate)
        (send-header (cons "Date" (current-time-rfc2822)))
        (for-each send-header headers)
        (if (and (input-port? content-input-port) (fxzero? length))
            (send-header (cons "Transfer-Encoding" "Chunked"))	   
            (send-header (cons "Content-Length" (number->string length 16))))
        (send terminate)
        (if (input-port? content-input-port)
            (let ((buffsz 1024))
              (let ((buffer (make-bytes buffsz)))
                (let loop ((cnt (read-bytes content-input-port buffer 0 buffsz)))
                  (if (eof-object? cnt)
                      (begin
                        (send "0")
                        (send terminate)(send terminate))
                      (begin
                        (send (number->string cnt 16))
                        (send terminate)
                        (write-bytes socket-output-port buffer 0 cnt)
                        (send terminate)
                        (loop  (read-bytes content-input-port buffer 0 buffsz)))))))
            (send terminate))))))


(define http-301-moved-permanently
  (lambda (loc socket-output-port)
    (let ((headers (list (cons "Location"  loc)
                         (cons "Content-Type" "text/html"))))
      (http-send-response "301 moved permanently" 
                          headers
                          socket-output-port
                          (open-input-bytes (string->bytes/utf-8 (string-append "Please follow <a href=\"" loc "\">Knozama</a>")))
                          0))))









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
