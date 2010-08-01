#lang racket

(provide fetch-url
	 transform-sxmldoc format-sxmldoc 
	 select at-least-one-child join-text)

(require knozama/web/uri
	 knozama/web/http/http
	 (planet lizorkin/ssax:2:0/ssax)
	 (planet neil/htmlprag:1:6)
	 (planet neil/csv:1:6))  

;; Common helper fn
(define (strip-cr s)
  (if (zero? (string-length s))
     s
     (let ((last-idx (sub1 (string-length s))))
       (if (not (char=? (string-ref s last-idx) #\newline))
	  s
	  (substring s 0 last-idx)))))

;; Common helper fn
(define (join-text textlst)
  (if (null? textlst)
     textlst
     (let loop ((text "")(textlst (reverse textlst)))
       (if (null? textlst) 
	  text
	  (when (pair? (car textlst)) ;; (*text* "some string")
	    (let ((value (strip-cr (cadar textlst))))
	      (loop (if (equal? text "")
		       value
		       (string-append text " " value))
		    (cdr textlst))))))))

(define (select pred kids)    
  (let filter ((kids kids) (accum '()))
    (if (null? kids)
       (reverse accum)
       (let ((kid (car kids)))
	 (cond 
	  ((null? kid)
	   (filter (cdr kids) accum))
	  ((pair? kid)
	   (cond 
	    ((pair? (car kid))
	     (filter (cdr kids) (filter kid accum)))
	    ((symbol? (car kid))
	     (if (pred (car kid))
		(filter (cdr kids) (cons kid accum))
		(filter (cdr kids) accum)))
	    ((null? (car kid))
	     (filter (cdr kids) (filter (cdr kid) accum)))
	    (else
	     (printf "Unexpected SXML element ~a.~n" (car kid))
	     kid)))
	  ((string? kid)
	   (if (pred kid)
	      (filter (cdr kids) (cons kid accum))
	      (filter (cdr kids) accum)))
	  (else
	   (printf "Unexpected SXML element ~a.~n" kid)
	   kid))))))    

(define (at-least-one-child pred kids)
  (let loop ((kids kids))
    (if (null? kids)
       #f
       (let ((kid (car kids)))
	 (if (pair? kid)
	    (if (pred (car kid))
	       #t
	       (loop (cdr kids)))
	    (loop (cdr kids)))))))

(define (mergeText kids)
  (let loop ((kids kids) (accum '()) (text '()))
    (when (null? kids)
      (cons accum (cons '*text* (string
				 (let ((kid (car kids)))
				   (if (eq? '*text*)
				      (loop (cdr kids) accum (cons (cadr kid) text))
				      (loop (cdr kids) (cons kid accum) text)))))))))

(define request-headers
  '("User-Agent: Mozilla/5.0 (X11; U; Linux x86_64; en-US; rv:1.7.12) Gecko/20050922 Fedora/1.0.7-1.1.fc3 Firefox/1.0.7"
    "Accept: text/xml,application/xml,application/xhtml+xml,text/html;q=0.9,text/plain;q=0.8,image/png,*/*;q=0.5"
    "Accept-Language: en-us,en;q=0.5"
					;      "Accept-Encoding: gzip,deflate"
    "Accept-Charset: ISO-8859-1,utf-8;q=0.7,*;q=0.7"))

;; Fetches a url as a SXML document
;; url -> sxml 
;;(define (fetch-url url)    
;;  (let ((inp (get-pure-port url request-headers)))  ;; Was impure but the HTTP headers were being put under top as strings -> invalid SXML  FIX ME RPR
;;    (html->sxml inp)))

(define (transform-sxmldoc sxmldoc xform)
  (pre-post-order sxmldoc xform))

(define (format-sxmldoc sxmldoc)
  (SRV:send-reply sxmldoc))

(define fetch-url
  (lambda (url)
    (let-values (((h p) (http-invoke 'GET url (list "Host: connect.pulaski.k12.wi.us")  #f)))
      (displayln h)
      (html->sxml p))))