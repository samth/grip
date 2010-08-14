#| Parse routines for RSS |#

#lang racket
 
 (provide
  fetch-rss 
  sx-items sx-title sx-description sx-link sx-pubdate sx-content:encoded
  sx-media-content sx-media-content-url sx-media-content-medium
  sx-media-content-width sx-media-content-height)
 
 (require
 (only-in (planet lizorkin/sxml:2:1/sxml)
	  sxpath)
 (only-in (planet lizorkin/ssax:2:0/ssax)
	  ssax:xml->sxml)
 (only-in (planet knozama/xml:1:0/util)
	  select-single-node-text)
 (only-in (planet knozama/webkit:1:0/web/uri)
	  uri-authority authority-host
	  make-uri uri->string) 
 (only-in (planet knozama/webkit:1:0/web/http/http11)
	  parse-http-response-line
	  response-line-code
	  http-invoke)
 (only-in (planet knozama/webkit:1:0/web/http/headers)
	  agent-header
	  host-header))

(define content-ns
  '(content . "http://purl.org/rss/1.0/modules/content/"))

(define media-ns
  '(media . "http://search.yahoo.com/mrss"))

(define base-nss
  (list media-ns))

(define sx-items
  (sxpath "/rss/channel/item"))

(define sx-title
  (select-single-node-text "/title" '()))

(define sx-description
  (select-single-node-text "/description" '()))

(define sx-link
  (select-single-node-text "/link" '()))

(define sx-pubdate
  (select-single-node-text "/pubDate" '()))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Following are RSS 1.0 module extension used in RSS2.0 by BLIPPR
;; Who are these people???
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define sx-content:encoded
  (select-single-node-text "content:encoded" (list content-ns)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Following media elements are Yahoo RSS2.0 extentions
;; See  http://search.yahoo.com/mrss
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; relative from an item
;; list of media:content elements
(define sx-media-content
  (sxpath "/media:group/media:content" base-nss))

(define sx-media-content-url
  (select-single-node-text "/media:url" base-nss))

(define sx-media-content-medium
  (select-single-node-text "/media:medium" base-nss))

(define sx-media-content-height
  (select-single-node-text "/media:height" base-nss))

(define sx-media-content-width
  (select-single-node-text "/media:width" base-nss))

 ;; fetch RSS2.0 content and parse to SXML
 ;; on parse error returns a '()
 (define fetch-rss
   (lambda (uri)
     (let-values (((hdrs ip) (http-invoke 'GET uri 
					`(,(host-header (authority-host (uri-authority uri)))
					  ,(agent-header "curl/7.16.4 (x86_64-redhat-linux-gnu) libcurl/7.16.4 OpenSSL/0.9.8b zlib/1.2.3 libidn/0.6.8")
					  "Accept: */*")
					#f)))
       (let ((http-resp (parse-http-response-line (car hdrs))))
	 (if (string=? (response-line-code http-resp) "200")
	    (call-with-exception-handler
	     (lambda (e)
	       (pretty-print "ERROR in Generic RSS fetch.")
	       (pretty-print (uri->string uri))
	       (pretty-print e)
	       (close-input-port ip))	       
	     (lambda ()
	       (let ((results (ssax:xml->sxml ip '())))
		 (close-input-port ip)
		 results)))
	    '())))))
