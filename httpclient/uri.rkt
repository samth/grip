;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Knozama's Amazon API Library
;; Copyright (C) 2007-2012  Raymond Paul Racine
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
	 parse-uri parse-authority uri->string
	 parse-http-path uri->start-line-path-string
	 http-path-path http-path-query http-path-fragment
	 Authority Authority-user Authority-host Authority-port
	 Uri Uri? Uri-scheme Uri-authority Uri-path Uri-query Uri-fragment)

(require 
 (only-in (planet rpr/prelude:1/std/opt)
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

(struct: Authority ([user : (Option String)]
		    [host : String]
		    [port : Integer])
	 #:transparent)

(struct: Uri ([scheme : String]
	      [authority : (U False Authority)]
	      [path : String]
	      [query : (Option String)]
	      [fragment : (Option String)])
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

(: make-uri (String (Option String) String Natural String (Option String) (Option String) ->  Uri))
(define (make-uri scheme user host port path query fragment)
  (let ((authority (Authority user host port)))
    (Uri scheme authority
	 (opt-string path "/")
	 (opt-string query)
	 (opt-string fragment))))

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
     (let ((user (Authority-user authority)))
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
  (and (equal? (Authority-user auth1)
	       (Authority-user auth2))
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
	  (write-char (assert (read-char ip) char?) op)
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

(: parse-uri (String -> (Option Uri)))
(define (parse-uri uri-str)
  (let ((ip (open-input-string uri-str)))
    (let ((scheme (parse-scheme ip)))
      (if (not scheme)
	  #f
	  (if (not (parse-char ip #\:))
	      #f
	      (let-values (((authority path) (parse-hier ip)))
		(let ((auth (if (string? authority)
				(parse-authority authority scheme)
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
(: parse-port (Input-Port -> (Option Natural)))
(define (parse-port ip)
  (let ((ch (read-char ip)))
    (if (eof-object? ch)  ;; no port
	#f
	(if (not (eq? ch #\:))
	    (error "Host must be optionally followed by a port.  Something else found.")
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
		    (let ((port (string->number port)))
		      (if (and (exact-integer? port)
			       (>= port 0))
			  port
			  (error "Invalid port (not a number?)"))))))))))

;;; Parse the host and optional port from a given string
;;; returns: (values host port)
(: parse-host (Input-Port -> (Option String)))
(define (parse-host ip)
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
	      host
	      #f)))))

(: parse-user (Input-Port -> (Option String)))
(define (parse-user ip)
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
	#f
	(get-output-string op))))

(: scheme-default-port (String -> (Option Integer)))
(define (scheme-default-port scheme)
  (cond
   ((string=? "http" scheme) 80)
   ((string=? "https" scheme) 443)
   (else #f)))

(: parse-authority (String String -> (Option Authority)))
(define (parse-authority auth-str scheme)
  (if (not (string? auth-str))
      #f
      (let ((ip (open-input-string auth-str)))
	(let ((user (parse-user ip)))
	  (let ((ip (if user
			ip
			(open-input-string auth-str)))) ;; restart parse
	    (let ((host (parse-host ip))
		  (port (let ((p (parse-port ip)))
			  (if p p (scheme-default-port scheme)))))
	      (if (and host port)
		  (Authority user host port)
		  #f)))))))

;; (rtd-printer-set! uri (lambda (uri outp)
;; 			(display "#<uri \"" outp)
;; 			(display (uri->string uri) outp)
;; 			(display "\">" outp)))

;; (rtd-printer-set! authority (lambda (auth outp)
;; 			      (display "#<authority \"" outp)
;; 			      (display (authority->string auth) outp)
;; 			      (display "\">" outp)))





;; (: test-auth-parse (-> Void))
;; (define (test-auth-parse)
;;   (define test-auth
;;     (list
;;      "www.amazon.com"
;;      "ray@www.amazon.com"
;;      "www.amazon.com:80"
;;      "ray@www.amazon.com:80"))

;;   (define test-auth-bad
;;     (list
;;      "ray@"
;;      "ray@80"
;;      ":90"
;;      "ray@:80"))


;;   (for-each (lambda: ((auth-str : String))
;; 	      (let ((auth (parse-authority auth-str)))
;; 		(if auth
;; 		   (begin
;; 		     (display "Username: ")
;; 		     (display (Authority-username auth))
;; 		     (display " Host: ")
;; 		     (display (Authority-host auth))
;; 		     (display " Port: ")
;; 		     (display (Authority-port auth))
;; 		     (newline))
;; 		   (displayln "No Authority"))))
;; 	    test-auth))


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
