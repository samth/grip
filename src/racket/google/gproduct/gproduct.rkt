#lang typed/racket/base

(provide
 product-search)

(require
 racket/pretty
 (only-in (planet knozama/xml:1/sxml)
	  sxpath)
 (only-in (planet knozama/webkit:1/web/http/header)
          make-header-string)
 (only-in (planet knozama/webkit:1/web/uri)
	  Uri make-uri parse-uri uri->string)
 (only-in (planet knozama/webkit:1/web/uri/url/param)
	  params->query encode-param-string)
 (only-in (planet knozama/webkit:1/web/http/http11)
	  HTTPConnection-in http-successful? http-close-connection http-invoke)
 (only-in (planet knozama/xml:1/sxml)
	  Sxml SXPath html->sxml xml->sxml extract-text extract-integer)
 (only-in "access.rkt"
	  load-key)
 (only-in "config.rkt"
	  gproduct-host gproduct-path std-query-params gproduct-nss))
	  	 
(: key String)
(define key (load-key #f))

(struct: SearchResult ([company : String]
		       [account : Integer]
		       [title : String]
		       [gtin  : String]
		       [link : String]) #:transparent)

;; HTTP Stuff

(: search-request-headers (Listof String))
(define search-request-headers  (list 
			    ;; (make-header-string "User-Agent" "Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/535.11 (KHTML, like Gecko) Chrome/17.0.963.2 Safari/535.11")
			    (make-header-string "User-Agent" "Googlebot/2.1 (+http://www.google.com/bot.html)")
			    (make-header-string "Accept" "text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8")
			    (make-header-string "Accept-Charset" "ISO-8859-1,utf-8;q=0.7,*;q=0.3")
			    (make-header-string "Accept-Encoding" "gzip")
			    (make-header-string "Accept-Language" "en-US,en;q=0.8")
			    (make-header-string "Cache-Control" "max-age=0")
			    (make-header-string "Connection" "keep-alive")))

(: query-q-param (String -> (Pairof String String)))
(define (query-q-param query)
  (cons "q" (encode-param-string query #t)))

(: access-key-param (Pairof String String))
(define access-key-param (cons "key" key)) 

(: make-query-uri ((Listof (Pair String String)) -> Uri))
(define (make-query-uri queries)
  (make-uri "https" #f gproduct-host 443 gproduct-path 
	    (params->query (append queries std-query-params)) ""))

(: gns (Listof (Pairof Symbol String)))
(define gns `((g . ,gproduct-nss) 
	 (a . "http://www.w3.org/2005/Atom")))

(: sx-prods SXPath)
(define sx-prods (sxpath "/a:feed/a:entry/g:product" gns))

(: sx-name SXPath)
(define sx-name (sxpath "/g:author/g:name/text()" gns))

(: sx-account SXPath)
(define sx-account (sxpath "/g:author/g:accountId/text()" gns))

(: sx-title SXPath)
(define sx-title (sxpath "/g:title/text()" gns))

(: sx-link SXPath)
(define sx-link (sxpath "/g:link/text()" gns))

(: sx-gtin SXPath)
(define sx-gtin (sxpath "/g:gtin/text()" gns))

(: parse-search-result (Sxml -> (Option SearchResult)))
(define (parse-search-result prod)
  (let ((gtin (extract-text (sx-gtin prod)))
      (title (extract-text (sx-title prod)))
      (name (extract-text (sx-name prod)))
      (account (extract-integer (sx-account prod)))
      (link (extract-text (sx-link prod))))
    (if (and account gtin)
       (SearchResult name account title gtin link)
       #f)))

(: product-search (String Params -> (Option (Listof (Option SearchResult)))))
(define (product-search query restrictions)
  (let ((url (make-query-uri (list (query-q-param query) access-key-param))))
    (pretty-print (uri->string url))
    (with-handlers ([exn:fail? 
		     (lambda (ex) #f)])
      (let ((conn (http-invoke 'GET url search-request-headers #f)))
	;; (pretty-print conn)
	(if (http-successful? conn)       
	   (let ((page (xml->sxml (HTTPConnection-in conn) '())))
	     (http-close-connection conn)
	      (pretty-print page)
	     (let ((prods (sx-prods page)))
	       ;;(pretty-print prods)
	       (if (andmap list? prods)
		  (map parse-search-result prods)
		  #f)))
	   (begin
	     (http-close-connection conn)
	     #f))))))
