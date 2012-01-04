#| Parse routines for RSS |#

#lang typed/racket/base
 
 (provide
  fetch-rss 
  sx-items sx-title sx-description sx-link sx-pubdate sx-content:encoded
  sx-media-content sx-media-content-url sx-media-content-medium
  sx-media-content-width sx-media-content-height)

(require/typed (planet lizorkin/ssax:2:0/ssax)
	       (ssax:xml->sxml (Input-Port (Listof String) -> (List Any))))
 
(require/typed
 (planet lizorkin/sxml:2:1/sxml)
 (sxpath (String (Listof (Pair Symbol String)) -> ((Listof Any) -> (Listof Any)))))

 (require
  racket/pretty
 (only-in (planet knozama/xml:1/util)
	  select-single-node-text)
 (only-in (planet knozama/webkit:1/web/uri)
	  uri-authority authority-host
	  uri make-uri uri->string) 
 (only-in (planet knozama/webkit:1/web/http/http11)
	  http-response-from-headers
	  response-line-code
	  http-invoke)
 (only-in (planet knozama/webkit:1/web/http/headers)
	  agent-header
	  host-header)
 (only-in "../../http/util.rkt"
	  ok-response?))

(: content-ns (Pair Symbol String))
(define content-ns
  '(content . "http://purl.org/rss/1.0/modules/content/"))

(: media-ns (Pair Symbol String))
(define media-ns
  '(media . "http://search.yahoo.com/mrss"))

(: base-nss (Listof (Pair Symbol String)))
(define base-nss
  (list media-ns))

(: sx-items ((Listof Any) -> (Listof Any)))
(define sx-items
  (sxpath "/rss/channel/item" '()))

(: sx-title ((Listof Any) -> String))
(define sx-title
  (select-single-node-text "/title" '()))

(: sx-description ((Listof Any) -> String))
(define sx-description
  (select-single-node-text "/description" '()))

(: sx-link ((Listof Any) -> String))
(define sx-link
  (select-single-node-text "/link" '()))

(: sx-pubdate ((Listof Any) -> String))
(define sx-pubdate
  (select-single-node-text "/pubDate" '()))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Following are RSS 1.0 module extension used in RSS2.0 by BLIPPR
;; Who are these people???
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(: sx-content:encoded ((Listof Any) -> String))
(define sx-content:encoded
  (select-single-node-text "content:encoded" (list content-ns)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Following media elements are Yahoo RSS2.0 extentions
;; See  http://search.yahoo.com/mrss
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; relative from an item
;; list of media:content elements
(: sx-media-content ((Listof Any) -> (Listof Any)))
(define sx-media-content
  (sxpath "/media:group/media:content" base-nss))

(: sx-media-content-url ((Listof Any) -> String))
(define sx-media-content-url
  (select-single-node-text "/media:url" base-nss))

(: sx-media-content-medium ((Listof Any) -> String))
(define sx-media-content-medium
  (select-single-node-text "/media:medium" base-nss))

(: sx-media-content-height ((Listof Any) -> String))
(define sx-media-content-height
  (select-single-node-text "/media:height" base-nss))

(: sx-media-content-width ((Listof Any) -> String))
(define sx-media-content-width
  (select-single-node-text "/media:width" base-nss))

 ;; fetch RSS2.0 content and parse to SXML
 ;; on parse error returns a '()
(: fetch-rss (uri -> (Listof Any)))
(define (fetch-rss uri)
  (let-values (((hdrs ip) (http-invoke 'GET uri 
				     `(,(agent-header "curl/7.16.4 (x86_64-redhat-linux-gnu) libcurl/7.16.4 OpenSSL/0.9.8b zlib/1.2.3 libidn/0.6.8")
				       "Accept: */*")
				     #f)))
    (if (ok-response? hdrs)
       (with-handlers ((exn:fail?  
			(lambda (e)
			  (pretty-print "ERROR in Generic RSS fetch.")
			  (pretty-print (uri->string uri))
			  (pretty-print e)
			  (close-input-port ip)
			  '())))
	 (let ((results (ssax:xml->sxml ip '())))
	   (close-input-port ip)
	   results))
       '())))
   