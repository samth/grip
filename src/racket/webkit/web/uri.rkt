;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Knozama's Amazon API Library
;; Copyright (C) 2007-2011  Raymond Paul Racine
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

#lang typed/racket/base

(provide make-uri
	 url-decode-string url-encode-string
	 parse-uri parse-authority uri->string
	 parse-http-path uri->start-line-path-string
	 http-path-path http-path-query http-path-fragment
	 Authority Authority-host Authority-port
	 Uri Uri-authority)

(require 
 (only-in (planet knozama/common:1/std/prelude)
	  ==>)
 (only-in (planet knozama/common:1/std/opt)
	  opt-apply-orelse)
 (only-in "uri/uricharset.rkt"
	  sub-delim-char?
	  pct-encoded-char?
	  digit-char?
	  pchar?
	  scheme-start-ch?
	  scheme-tail-ch?
	  hex-char?
	  encode-char
	  unreserved-char?))

;; Encode the given string
;; space-as-plus boolean denotes if spaces should be encoded with '+' or %20.
(: url-encode-string (String Boolean -> String))
(define url-encode-string
  (lambda (s space-as-plus)
    (let ((is (open-input-string s))
	(os (open-output-string)))
      (let: loop : String ((ch : (U Char EOF) (read-char is)))
	  (cond
	   ((eof-object? ch) (get-output-string os))
	   ((unreserved-char? ch) (begin (write-char ch os)
				     (loop (read-char is))))
	   (else (if (and space-as-plus
		       (char=? ch #\space))
		    (begin (write-char #\+ os)
		       (loop (read-char is)))
		    (begin (write-string (encode-char ch) os)
		       (loop (read-char is))))))))))

;; Read from the input port until eof or delim char
;; URL decode as well.
;; if delim = #f then process till eof
(: url-decode-string (Input-Port Char Boolean -> String))
(define url-decode-string
  (lambda (ip delim decode-plus?)
    (let ((op (open-output-string)))
      (let: loop : String ((ch : (U Char EOF) (read-char ip)))
	  (if (or (eof-object? ch)
		(and delim (char=? ch delim)))
	     (get-output-string op)
	     (if (char=? #\% ch)
		(let ((ch1 (read-char ip)))
		  (if (and (not (eof-object? ch1))
			(hex-char? ch1))
		     (let ((ch2 (read-char ip)))
		       (if (and (not (eof-object? ch2))
			     (hex-char? ch2))
			  (begin ;; use (let ((buff (make-string 2))) ???
			    (write-char (integer->char (assert (string->number (list->string (list ch1 ch2)) 16) 
							       exact-integer?))
					op)
			    (loop (read-char ip)))
			  (begin              ;; got %d? so write '%' and digit and carryon
			    (write-char #\% op)
			    (write-char ch1 op)
			    (unless (eof-object? ch2)
			      (write-char ch2 op))
			    (loop (read-char ip)))))
		     (begin                   ;; got %? so write them and carryon
		       (write-char #\% op)
		       (unless (eof-object? ch1)
			 (write-char ch1 op))
		       (loop (read-char ip)))))
		(begin
		  (unless (eof-object? ch)
		    (if (and decode-plus?
			  (char=? ch #\+))
		       (write-char #\space op)
		       (write-char ch op)))
		  (loop (read-char ip)))))))))

;; all strings or #f
(struct: Authority ((username : (Option String))
		    (host : String)
		    (port : (Option Integer)))
	 #:transparent)

;;all strings or #f, each string is a major piece of the uri.
(struct: Uri ((scheme : String)
	      (authority : (U False Authority))
	      (path : String)
	      (query : (Option String))
	      (fragment : (Option String)))
	 #:transparent)

(: null-string? (String -> Boolean))
(define null-string?
  (lambda (s)
    (if (string? s)
       (zero? (string-length s))
       #f)))

(define-syntax opt-string
  (syntax-rules ()
    ((_ str)
     (if (and (string? str) (not (zero? (string-length str))))
	str
	#f))
    ((_ str alt)
     (if (and (string? str) (not (zero? (string-length str))))
	str
	alt))))

(: make-uri (String (Option String) String (U False Natural) String (Option String) (Option String) -> (Option Uri)))
(define make-uri
  (lambda (scheme user host port path query fragment)
    (let ((authority (Authority user host port)))
      (if (null-string? scheme)
	 #f
	 (Uri scheme authority
	      (opt-string path "/")
	      (opt-string query)
	      (opt-string fragment))))))

(: maybe ((Option String) String -> String))
(define (maybe field prefix)
  (opt-apply-orelse field (lambda: ((field : String))
			    (string-append prefix field))
		    ""))

(: uri->start-line-path-string (Uri -> String))
(define uri->start-line-path-string
  (lambda (uri)
    (string-append
     (Uri-path uri)
     (maybe (Uri-query uri) "?")
     (maybe (Uri-fragment uri) "#"))))

(: uri->string (Uri -> String))
(define (uri->string uri)
  (string-append
   (Uri-scheme uri)
   ":"
   (let ((auth (authority->string (Uri-authority uri))))
     (if auth
	(string-append "//" auth)
	""))
   (Uri-path uri)
   (maybe (Uri-query uri) "?")
   (maybe (Uri-fragment uri) "#")))

(: authority->string ((U False String Authority) -> (Option String)))
(define (authority->string authority)
  (cond
   ((string? authority) authority)
   ((Authority? authority)
    (string-append
     (let ((user (Authority-username authority)))
       (if (cond
	   ((boolean? user) user)
	   ((string? user) (> (string-length user) 0)))
	  (string-append user "@")
	  ""))
     (Authority-host authority)
     (let ((port (Authority-port authority)))
       (if port
	  (string-append ":" (number->string port))
	  ""))))
   (else #f)))

;; Two authororities are equal if they're record values are equal.
(: authority-equal? (Authority Authority -> Boolean))
(define (authority-equal? auth1 auth2)
  (and (equal? (Authority-username auth1)
	     (Authority-username auth2))
     (equal? (Authority-host auth1)
	     (Authority-host auth2))
     (eqv? (Authority-port auth1)
	   (Authority-port auth2))))

;; Read chars while valid or eof-object?
;; Place valid chars in output string port
;; First invalid char is left on the input port
;; returns: number of valid chars read from input port.

(: read-valid (Input-Port (Char -> Boolean) Output-Port -> Integer))
(define (read-valid ip valid? op)
  (let loop ((ch (peek-char ip)) (cnt 0))
    (if (or (eof-object? ch)
	  (not (valid? ch)))
       cnt
       (begin
	 (write-char (assert (read-char ip) char?)  op)
	 (loop (peek-char ip) (add1 cnt))))))

;; (input-port?  output-port?) -> boolean?)
;; parse the "tail" of a scheme
;; i.e., the rest of the scheme string given that
;; the start char of the scheme was valid.
;; returns: # of chars read
(: parse-scheme-tail (Input-Port Output-Port -> Integer))
(define (parse-scheme-tail ip op)
  (read-valid ip scheme-tail-ch? op))

(: parse-scheme (Input-Port -> (Option String)))
(define (parse-scheme ip)
  (let ((op (open-output-string)))
    (let ((ch (peek-char ip)))
      (if (eof-object? ch)
	 #f
	 (let ((ch (assert ch char?)))
	   (if (not (scheme-start-ch? ch))
	      #f
	      (begin (read-char ip)
		 (write-char ch op)
		 (parse-scheme-tail ip op)
		 (get-output-string op))))))))


;; lex a character of value chtok
;; returns: #f if the next character is not a chtok
(: parse-char (Input-Port Char -> Boolean))
(define (parse-char ip chtok)
  (let ((ch (peek-char ip)))
    (if (eof-object? ch)
       #f
       (if (eq? ch chtok)
	  (begin
	    (read-char ip)
	    #t)
	  #f))))

(: parse-authority-opaque (Input-Port -> String))
(define (parse-authority-opaque ip)
  (let ((op (open-output-string)))
    (read-valid ip (lambda (ch)
		     (case ch
		       ((#\/ #\? #\#) #f)
		       (else #t)))
		op)
    (get-output-string op)))

(: parse-path-abempty (Input-Port -> String))
(define (parse-path-abempty ip)
    (let ((op (open-output-string)))
      (let ((ch (peek-char ip)))
	(if (or (eof-object? ch)
	      (eq? ch #\?)
	      (eq? ch #\#))
	   ""
	   (if (not (eq? ch #\/))
	      (error "A URI with an authority can only have an absolute path.")
	      (begin (read-char ip)
		 (write-char ch op)
		 (let ((ch (peek-char ip)))
		   (if (eq? ch #\/)
		      (error "Absolute path must have a none empty segment.  i.e., // is illegal")
		      (read-valid ip
				  (lambda (ch)
				    (or
				     (pchar? ch)
				     (eq? ch #\/)))
				  op))))))
	(get-output-string op))))

(: parse-path-absolute (Input-Port -> String))
(define (parse-path-absolute ip)
    (let ((op  (open-output-string)))
      (write-char #\/ op)
      ;; first segment must not have a ':'
      (read-valid ip
		  (lambda (ch)
		    (and
		     (not (eq? ch #\:))
		     (pchar? ch)))
		  op)
      (read-valid ip
		  (lambda (ch)
		    (or
		     (pchar? ch)
		     (eq? ch #\/)))
		  op)
      (get-output-string op)))

(: parse-path-rootless (Input-Port -> String))
(define parse-path-rootless
  (lambda (ip)
    (let ((op (open-output-string)))
      (read-valid ip
		  (lambda (ch)
		    (or
		     (pchar? ch)
		     (eq? ch #\/)))
		  op)
      (get-output-string op))))

;; returns 2 values
;;  1) opaque authority string or #f
;;  2) path
(: parse-hier (Input-Port -> (values (Option String) String)))
(define parse-hier
  (lambda (ip)
    (let ((ch (peek-char ip)))
      (if (eof-object? ch)
	 (values #f "")
	 (if (eq? ch #\/)
	    (begin
	      (read-char ip)
	      (if (eq? (peek-char ip) #\/)
		 (begin
		   (read-char ip)
		   (let ((authority (parse-authority-opaque ip)))
		     (let ((path-abempty (parse-path-abempty ip)))
		       (values authority path-abempty))))
		 (values #f (parse-path-absolute ip))))
	    (values #f (parse-path-rootless ip)))))))

(: parse-query-or-fragment (Input-Port Char -> (Option String)))
(define (parse-query-or-fragment ip signal-char)
  (let ((ch (peek-char ip)))
    (if (eof-object? ch)
       #f
       (if (eq? ch signal-char)
	  (let ((op (open-output-string)))
	    (read-char ip) ;; consume signal char
	    (read-valid ip
			(lambda (ch)
			  (or
			   (pchar? ch)
			   (eq? ch #\?)
			   (eq? ch #\/)))
			op)
	    (get-output-string op))
	  #f))))

(: parse-uri (String -> Uri))
(define (parse-uri uri-str)
  (let ((ip (open-input-string uri-str)))
    (let ((scheme (parse-scheme ip)))
      (if (not scheme)
	 (error "Invalid URI.  manditory scheme is missing.")
	 (if (not (parse-char ip #\:))
	    (error "Invalid URI.  scheme must be delimited by a ':'.")
	    (let-values (((authority path) (parse-hier ip)))
	      (let ((auth (if (string? authority)
			   (parse-authority authority)
			   #f)))
		(let ((query (parse-query-or-fragment ip #\?)))
		  (let ((fragment (parse-query-or-fragment ip #\#)))
		    (Uri scheme auth path query fragment))))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Routines to parse a HTTP request start line path.				 ;;
;; Given start-line "GET /a/b/c/d.txt?x=2#one" 				 ;;
;; (parse-http-start-line-path start-line) => ("a/b/c/d.txt"  "x=2" "one")	 ;;
;; The other routines are just sugar to extract out the path, query or fragment ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(: http-path-path ((Listof String) -> String))
(define http-path-path
  (lambda (slpath)
    (car slpath)))

(: http-path-query ((Listof String) -> String))
(define http-path-query
  (lambda (slpath)
    (cadr slpath)))

(: http-path-fragment ((Listof String) -> String))
(define http-path-fragment
  (lambda (slpath)
    (caddr slpath)))       

(: parse-http-path (String -> (Listof (Option String))))
(define parse-http-path
  (lambda (path-str)
    (let ((ip (open-input-string path-str)))
      (let-values (((auth path) (parse-hier ip)))
	(let ((query (parse-query-or-fragment ip #\?)))
	  (let ((fragment (parse-query-or-fragment ip #\#)))
	    (list path query fragment)))))))

;; Parse out the port string.
;; Assumes leading ':' has been consumed.
(: parse-port (Input-Port -> Integer))
(define (parse-port ip)
  (let ((op  (open-output-string))
      (ch  (peek-char ip)))
    (if (or (eof-object? ch)
	  (not (digit-char? (assert ch char?))))
       (error "Missing port number or extraneous characters where port number was expected.")
       (let ((port (begin (read-valid ip
				(lambda (ch)
				  (digit-char? ch))
				op)
		    (get-output-string op))))
	 (assert (string->number port) exact-integer?)))))

;;; Parse the host and optional port from a given string
;;; returns: (values host port)
(: parse-host-port (Input-Port -> (values String (U False Integer))))
(define parse-host-port
  (lambda (ip)
    (let ((op  (open-output-string)))
      (if (eof-object? (peek-char ip))
	 (error "URI missing required host.")
	 (let ((host (begin
		     (read-valid ip
				 (lambda (ch)
				   (or
				    (unreserved-char? ch)
				    (pct-encoded-char? ch)
				    (sub-delim-char? ch)))
				 op)
		     (get-output-string op))))
	   (if (> (string-length host) 0)
	      (let ((ch (read-char ip)))
		(if (eof-object? ch)  ;; no port
		   (values host #f)
		   (if (not (eq? ch #\:))
		      (error "Host must be optionally followed by a port.  Something else found.")
		      (let ((port (parse-port ip)))
			(values host port)))))
	      (error "Manditory host is missing.")))))))

(: authority-with-username? (String -> Boolean))
(define authority-with-username?
  (lambda (auth)
    (let ((ip (open-input-string auth)))
      (let loop ((ch (read-char ip)))
	(cond
	 ((eof-object? ch) #f)
	 ((eq? ch #\@) #t)
	 (else
	  (loop (read-char ip))))))))

(: parse-authority (String -> Authority))
(define (parse-authority auth-str)
  (if (not (string? auth-str))
     #f
     (let ((ip (open-input-string auth-str)))
       (if (authority-with-username? auth-str)
	  (let ((op  (open-output-string)))
	    (read-valid ip
			(lambda (ch)
			  (or
			   (unreserved-char? ch)
			   (pct-encoded-char? ch)
			   (sub-delim-char? ch)
			   (eq? ch #\:)))
			op)
	    (if (not (eq? (read-char ip) #\@))
	       (error "Invalid username.")
	       (let ((username (get-output-string op)))
		 (let-values (((host port) (parse-host-port ip)))
		   (Authority username host port)))))
	  (let-values (((host port) (parse-host-port ip)))
	    (Authority #f host port))))))


;; (rtd-printer-set! uri (lambda (uri outp)
;; 			(display "#<uri \"" outp)
;; 			(display (uri->string uri) outp)
;; 			(display "\">" outp)))

;; (rtd-printer-set! authority (lambda (auth outp)
;; 			      (display "#<authority \"" outp)
;; 			      (display (authority->string auth) outp)
;; 			      (display "\">" outp)))






;;          (define test-auth
;;            (list
;;             "www.amazon.com"
;;             "ray@www.amazon.com"
;;             "www.amazon.com:80"
;;             "ray@www.amazon.com:80"))

;;          (define test-auth-bad
;;            (list
;;             "ray@"
;;             "ray@80"
;;             ":90"
;;             "ray@:80"))

;;          (for-each (lambda (auth-str)
;;                      (let ((auth (parse-authority auth-str)))
;;                        (display "Username: ")
;;                        (display (authority-username auth))
;;                        (display " Host: ")
;;                        (display (authority-host auth))
;;                        (display " Port: ")
;;                        (display (authority-port auth))
;;                        (newline)))
;;                    test-auth)


;; http://ecs.amazonaws.com/onca/xml?
;; Service=AWSECommerceService&
;; Operation=ItemSearch&
;; AWSAccessKeyId=[Access Key ID]&
;; AssociateTag=[ID]&
;; SearchIndex=Apparel&
;; Keywords=Shirt

;; (uri->string (parse-uri (uri->string
;;                          (make-uri "http"
;;                                    "" "ecs.amazonaws.com" #f
;;                                    "/onca/xml"          
;;                                    "Service=AWSECommerceService&Operation=ItemSearch&AWSAccessKeyId=[Access Key ID]&AssociateTag=[ID]&SearchIndex=Apparel&Keywords=Shirt"
;;                                    ""))))


;; (uri-authority (make-uri "http"
;;                          "" "ecs.amazonaws.com" 8080
;;                          "/onca/xml"
;;                          "Service=AWSECommerceService&Operation=ItemSearch&AWSAccessKeyId=[Access Key ID]&AssociateTag=[ID]&SearchIndex=Apparel&Keywords=Shirt"
;;                          "")) )
