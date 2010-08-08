#lang racket
 
(provide booktour-search)

(require
 (only-in (planet lizorkin/ssax:2:0/ssax)
	  ssax:xml->sxml)
 (only-in (planet neil/htmlprag:1:6)
	  html->sxml)
 (only-in knozama/web/uri/url/param
	  parms->query)
 (only-in knozama/web/uri
	  make-uri uri->string)
 (only-in knozama/web/http/http
	  parse-http-response-line
	  response-line-code
	  http-invoke)
 (only-in knozama/web/http/headers
	  host-header))

(define booktour-host "booktour.com")
(define booktour-path "/readers/rss_custom")

;; time in days
;; dist in miles
;; categories: Home=All
(define booktour-search
  (lambda (zipcode distance category time)
    (let ((parms `(("distance" . ,distance)
		 ("zip"      . ,zipcode)
		 ("category" . ,category)
		 ("time"     . ,time))))       
      (let ((uri (make-uri "http" #f booktour-host #f booktour-path (parms->query parms) "")))
	(let-values (((hdrs ip) (http-invoke 'GET uri `(,(host-header booktour-host)) #f)))
	  (call-with-exception-handler
	   (lambda (e)
	     (display "Booktour call failed.")
	     (displayln e)
	     (close-input-port ip)
	     '())
	   (lambda ()
	     (let ((http-resp (parse-http-response-line (car hdrs))))
	       (if (string=? (response-line-code http-resp) "200")
		  (let ((results (ssax:xml->sxml ip '())))
		    (close-input-port ip)
		    results)
		  '())))))))))

;; http://booktour.com/readers/rss_custom/?distance=50&zip=33496&category=Home&time=28