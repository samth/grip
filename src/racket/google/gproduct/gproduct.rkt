#lang typed/racket/base

(provide)

(require
 racket/pretty
 (only-in (planet knozama/xml:1/sxml)
	  sxpath)
 (only-in (planet knozama/webkit:1/web/http/header)
          make-header-string)
 (only-in (planet knozama/webkit:1/web/uri)
	  Uri make-uri parse-uri)
 (only-in (planet knozama/webkit:1/web/uri/url/param)
	  params->query encode-param-string)
 (only-in (planet knozama/webkit:1/web/http/http11)
	  HTTPConnection-in http-successful? http-close-connection http-invoke)
 (only-in (planet knozama/xml:1/sxml)
	  Sxml SXPath xml->sxml extract-text extract-integer)
 (only-in "access.rkt"
	  load-key)
 (only-in "config.rkt"
	  gproduct-host gproduct-path std-query-params))
	  	 
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
			    ;;(make-header-string "Accept-Encoding" "gzip,deflate,sdch")
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

;; Response parsing stuff

(: sx-prods SXPath)
(define sx-prods (sxpath "/feed/entry/product" '()))

(: sx-name SXPath)
(define sx-name (sxpath "/author/name/text()" '()))

(: sx-account SXPath)
(define sx-account (sxpath "/author/accountid/text()" '()))

(: sx-title SXPath)
(define sx-title (sxpath "/title/text()" '()))

(: sx-link SXPath)
(define sx-link (sxpath "/link/text()" '()))

(: sx-gtin SXPath)
(define sx-gtin (sxpath "/gtin/text()" '()))

(: parse-search-result (Sxml -> (Option SearchResult)))
(define (parse-search-result page)
  (let ((prods (sx-prods page)))
    (let ((gtin (extract-text (sx-gtin prods)))
	(title (extract-text (sx-title prods)))
	(name (extract-text (sx-name prods)))
	(account (extract-integer (sx-account prods)))
	(link (extract-text (sx-link prods))))
      (pretty-print (sx-link prods))
      (pretty-print name)
      (pretty-print gtin)
      (pretty-print account)
      (if (and account gtin)
	 (SearchResult name account title gtin link)
	 #f))))

(: product-search (String -> (Option SearchResult)))
(define (product-search query)
  (let ((url (make-query-uri (list (query-q-param query) access-key-param))))
    (with-handlers ([exn:fail? 
		     (lambda (ex) #f)])
      (let ((conn (http-invoke 'GET url search-request-headers #f)))
	(pretty-print conn)
	(if (http-successful? conn)       
	   (let ((page (xml->sxml (HTTPConnection-in conn) '("http://www.google.com/shopping/api/schemas/2010"))))
	     (pretty-print page)
	     (http-close-connection conn)
	     (parse-search-result page))
	   (begin
	     (http-close-connection conn)
	     #f))))))
