#lang racket
 
 (provide 
  feedburner-rss)
 
 (require
  (only-in knozama/web/rss/rss20/rss
	   fetch-rss)
  (only-in knozama/web/uri
	   make-uri)
  (only-in knozama/web/uri/url/param
	   parms->query))
  
(define feedburner-host
  "feedproxy.feedburner.com")

(define parms (parms->query '()))

(define feedburner-rss
  (lambda (feed)
    (let ((uri (make-uri "http" #f  feedburner-host #f feed parms "")))
      (fetch-rss uri))))

