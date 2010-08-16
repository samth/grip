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

#lang racket/base

(provide make-uri
	 url-decode-string url-encode-string
	 parse-uri parse-authority uri->string
	 parse-http-path uri->start-line-path-string
	 http-path-path http-path-query http-path-fragment
	 (struct-out authority)
	 (struct-out uri))

;;(provide/contract (make-uri (-> string? (or/c string? boolean?) 
;;				string? (or/c number? boolean?) string? string? string? string?)))

(require 
 (only-in (planet knozama/common:1:0/std/prelude)
	  fx1+)
 "uri/uricharset.rkt")

;; Encode the given string
;; space-as-plus boolean denotes if spaces should be encoded with '+' or %20.
;; string -> boolean -> string
(define url-encode-string
  (lambda (s space-as-plus)
    (let ((is (open-input-string s))
	(os (open-output-string)))
      (let loop ((ch (read-char is)))
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
(define url-decode-string
  (lambda (ip delim decode-plus?)
    (let ((op (open-output-string)))
      (let loop ((ch (read-char ip)))
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
			(begin
			  (write-char (integer->char (string->number (list->string (list ch1 ch2)) 
								     16)) op) ;; use (let ((buff (make-string 2))) ???
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
(struct authority (username host port))

;;all strings or #f, each string is a major piece of the uri.
(struct uri (scheme authority path query fragment))

(define null-string?
  (lambda (s)
    (if (string? s)
       (zero? (string-length s))
       #f)))

(define make-uri
  (lambda (scheme user host port path query fragment)
    ;; (assert (and
    ;; 	     (string? scheme)
    ;; 	     (or (string? user)
    ;; 		(and (boolean? user)
    ;; 		   (not user)))
    ;; 	     (string? host)
    ;; 	     (or (number? port)
    ;; 		(and (boolean? port)
    ;; 		   (not port)))
    ;; 	     (string? path)
    ;; 	     (string? query)
    ;; 	     (string? fragment)))
    (let ((sport (if (number? port)
		  (number->string port)
		  port)))                               
      (let ((authority (authority (if (null-string? user)
				   #f user)
				host (if (null-string? sport) #f sport))))
	(if (null-string? scheme)
	   #f
	   (uri scheme authority
		(if (null-string? path)
		   "/"
		   path)
		(if (null-string? query)
		   #f
		   query)
		(if (null-string? fragment)
		   #f
		   fragment)))))))

(define maybe
  (lambda (field prefix)
    (if field (string-append prefix field) "")))

(define uri->start-line-path-string
  (lambda (uri)
    (string-append
     (uri-path uri)
     (maybe (uri-query uri) "?")
     (maybe (uri-fragment uri) "#"))))

(define uri->string
  (lambda (uri)
    (string-append
     (uri-scheme uri)
     ":"
     (let ((auth (authority->string (uri-authority uri))))
       (if auth
	  (string-append "//" auth)
	  ""))
     (uri-path uri)
     (maybe (uri-query uri) "?")
     (maybe (uri-fragment uri) "#"))))

(define authority->string
  (lambda (authority)
    (string-append
     (let ((user (authority-username authority)))
       (if (cond
	    ((boolean? user) user)
	    ((string? user) (> (string-length user) 0)))
	   (string-append user "@")
	   ""))
     (authority-host authority)
     (let ((port (authority-port authority)))
       (if port
	   (string-append ":"  port) ;;(number->string port))
	   "")))))

;; Two authororities are equal if they're record values are equal.
(define authority-equal?
  (lambda (auth1 auth2)
    (and (equal? (authority-username auth1)
		 (authority-username auth2))
	 (equal? (authority-host auth1)
		 (authority-host auth2))
	 (eqv? (authority-port auth1)
	       (authority-port auth2)))))

;; Read chars while valid or eof-object?
;; Place valid chars in output string port
;; First invalid char is left on the input port
;; returns: number of valid chars read from input port.

(define read-valid
  (lambda (ip valid? op)
    (let loop ((ch (peek-char ip)) (cnt 0))
      (if (or (eof-object? ch)
	      (not (valid? ch)))
	  cnt
	  (begin
	    (write-char (read-char ip) op)
	    (loop (peek-char ip) (fx1+ cnt)))))))

;; (input-port?  output-port?) -> boolean?)
;; parse the "tail" of a scheme
;; i.e., the rest of the scheme string given that
;; the start char of the scheme was valid.
;; returns: # of chars read
(define parse-scheme-tail
  (lambda (ip op)
    (read-valid ip scheme-tail-ch? op)))

(define parse-scheme
  (lambda (ip)
    (let ((op (open-output-string)))
      (let ((ch (peek-char ip)))
	(if (not (scheme-start-ch? ch))
	    #f
	    (begin
	      (write-char (read-char ip) op)
	      (parse-scheme-tail ip op)
	      (get-output-string op)))))))


;; lex a character of value chtok
;; returns: #f if the next character is not a chtok
(define (parse-char ip chtok)
  (let ((ch (peek-char ip)))
    (if (eof-object? ch)
	#f
	(if (eq? ch chtok)
	    (begin
	      (read-char ip)
	      #t)
	    #f))))

(define parse-authority-opaque
  (lambda (ip)
    (let ((op (open-output-string)))
      (read-valid ip (lambda (ch)
		       (case ch
			 ((#\/ #\? #\#) #f)
			 (else #t)))
		  op)
      (get-output-string op))))

(define parse-path-abempty
  (lambda (ip)
    (let ((op (open-output-string)))
      (let ((ch (peek-char ip)))
	(if (or (eof-object? ch)
		(eq? ch #\?)
		(eq? ch #\#))
	    ""
	    (if (not (eq? ch #\/))
		(error "A URI with an authority can only have an absolute path.")
		(begin
		  (write-char (read-char ip) op)
		  (let ((ch (peek-char ip)))
		    (if (eq? ch #\/)
			(error "Absolute path must have a none empty segment.  i.e., // is illegal")
			(read-valid ip
				    (lambda (ch)
				      (or
				       (pchar? ch)
				       (eq? ch #\/)))
				    op))))))
	(get-output-string op)))))

(define parse-path-absolute
  (lambda (ip)
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
      (get-output-string op))))

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
;;  1) authority or #f
;;  2) path
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

(define parse-query-or-fragment
  (lambda (ip signal-char)
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
	     #f)))))

(define parse-uri
  (lambda (uri-str)
    (let ((ip (open-input-string uri-str)))
      (let ((scheme (parse-scheme ip)))
	(if (not scheme)
	   (error "Invalid URI.  manditory scheme is missing.")
	   (if (not (parse-char ip #\:))
	      (error "Invalid URI.  scheme must be delimited by a ':'.")
	      (let-values (((authority path) (parse-hier ip)))
		(display "Auth: ") (display authority)(newline)
		(display "Path: ") (display path)(newline)
		(let ((query (parse-query-or-fragment ip #\?)))
		  (let ((fragment (parse-query-or-fragment ip #\#)))
		    (uri scheme authority path query fragment))))))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Routines to parse a HTTP request start line path.				 ;;
;; Given start-line "GET /a/b/c/d.txt?x=2#one" 				 ;;
;; (parse-http-start-line-path start-line) => ("a/b/c/d.txt"  "x=2" "one")	 ;;
;; The other routines are just sugar to extract out the path, query or fragment ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define http-path-path
  (lambda (slpath)
    (car slpath)))

(define http-path-query
  (lambda (slpath)
    (cadr slpath)))

(define http-path-fragment
  (lambda (slpath)
    (caddr slpath)))       

(define parse-http-path
  (lambda (path-str)
    (let ((ip (open-input-string path-str)))
      (let-values (((auth path) (parse-hier ip)))
	(let ((query (parse-query-or-fragment ip #\?)))
	  (let ((fragment (parse-query-or-fragment ip #\#)))
	    (list path query fragment)))))))

;; Parse out the port string.
;; Assumes leading ':' has been consumed.
(define parse-port
  (lambda (ip)
    (let ((op  (open-output-string)))
      (if (or (eof-object? (peek-char ip))
	    (not (digit-char? (peek-char ip))))
	 (error "Missing port number or extraneous characters where port number was expected.")
	 (let ((port (begin
		     (read-valid ip
				 (lambda (ch)
				   (digit-char? ch))
				 op)
		     (get-output-string op))))
	   (string->number port))))))

;;; Parse the host and optional port from a given string
;;; returns: (values host port)
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

(define authority-with-username?
  (lambda (auth)
    (let ((ip (open-input-string auth)))
      (let loop ((ch (read-char ip)))
	(cond
	 ((eof-object? ch) #f)
	 ((eq? ch #\@) #t)
	 (else
	  (loop (read-char ip))))))))

(define parse-authority
  (lambda (auth-str)
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
		     (authority username host port)))))
	    (let-values (((host port) (parse-host-port ip)))
	      (authority #f host port)))))))


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
