#lang typed/racket/base

(provide)

(require
 racket/pretty
 (only-in (planet knozama/webkit:1/web/http/header)
          make-header-string)
 (only-in (planet knozama/webkit:1/web/uri)
	  Uri make-uri parse-uri)
 (only-in (planet knozama/webkit:1/web/uri/url/param)
	  params->query encode-param-string)
 (only-in (planet knozama/webkit:1/web/http/http11)
	  HTTPConnection-in http-successful? http-close-connection http-invoke)
 (only-in (planet knozama/xml:1/sxml)
	  Sxml SXPath html->sxml)
 (only-in "access.rkt"
	  load-key)
 (only-in "config.rkt"
	  gproduct-host gproduct-path std-query-params))
	  	 
(: key String)
(define key (load-key #f))

(struct: SearchResult ([company : String]
		       [account : Integer]
		       [title : String]
		       [link : String]))

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

(: make-query-uri ((Listof (Pair String String)) -> Uri))
(define (make-query-uri queries)
  (make-uri "https" #f gproduct-host 443 gproduct-path 
	    (params->query (append queries std-query-params)) ""))

(: product-search (String -> (Option SearchResult)))
(define (product-search query)
  (let ((url (make-query-uri (list (cons "q" (encode-param-string query #f))))))
    (pretty-print url)
    #f))

;; (: test-uri Uri)
;; (define test-uri
;;   (assert (parse-uri (string-append "https://www.googleapis.com/shopping/search/v1/public/products?country=US&brand=smead&q=smead+153L&fields=entry/s:product(s:author,s:gtin,s:title,s:link)&alt=atom&key=" key))))

;; (: test (-> (Option Sxml)))
;; (define (test)
;;   (pretty-print test-uri)
;;   (let ((conn (http-invoke 'GET test-uri search-request-headers #f)))
;;     (pretty-print conn)
;;     (if (http-successful? conn)
;;        (let ((page (html->sxml (HTTPConnection-in conn))))
;; 	 (http-close-connection conn)
;; 	 page)
;;        (begin
;; 	 (http-close-connection conn)
;; 	 #f))))
