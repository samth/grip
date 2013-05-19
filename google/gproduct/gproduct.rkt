;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Knozama's Amazon API Library
;; Copyright (C) 2012  Raymond Paul Racine
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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#lang typed/racket/base

(provide
 SearchResult SearchResult? 
 SearchResult-company SearchResult-account SearchResult-brand
 SearchResult-title SearchResult-gtin SearchResult-link SearchResult-gid
 SearchResult-price
 product-search)

(require
 racket/pretty
 (only-in httpclient/header
          make-header Headers)
 (only-in net/uri/url/url
	  Authority QParam QParams
          Url parse-url url->string)
 (only-in httpclient/param
          params->query encode-param-string Param Params)
 (only-in httpclient/http11
          HTTPConnection-in http-successful? http-close-connection http-invoke)
 (only-in format/xml/sxml
          Sxml SXPath sxpath html->sxml xml->sxml extract-text extract-integer)
 (only-in "access.rkt"
          load-key)
 (only-in "config.rkt"
          gproduct-host gproduct-path std-query-params gproduct-nss))

(: key String)
(define key (load-key #f))

(struct: SearchResult ([company : String]
		       [account : Integer]
		       [title   : String]
		       [gtin    : String]
		       [gid     : String]
		       [link    : String]
		       [brand   : String]
		       [price   : String]))

;; HTTP Stuff
(: search-request-headers Headers)
(define search-request-headers  
  (list 
   (make-header "User-Agent" "Googlebot/2.1 (+http://www.google.com/bot.html)")
   (make-header "Accept" "text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8")
   ;; (make-header-string "Accept-Charset" "ISO-8859-1,utf-8;q=0.7,*;q=0.3")
   (make-header "Accept-Encoding" "gzip")
   ;;(make-header-string "Accept-Language" "en-US,en;q=0.8")
   (make-header "Cache-Control" "max-age=0")
   (make-header "Connection" "close")))

(: query-q-param (String -> Param))
(define (query-q-param query)
  (cons "q" (encode-param-string query #t)))

(: access-key-param Param)
(define access-key-param (cons "key" key)) 

(: make-query-uri (QParams -> Url))
(define (make-query-uri qparams)
  ;; (make-uri "https" #f gproduct-host 443 gproduct-path 
  ;; 	    (params->query (append queries std-query-params)) "")
  (Url 'HTTPS (Authority #f gproduct-host 443) 
       gproduct-path 
       (append qparams std-query-params) #f))

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

(: sx-gid SXPath)
(define sx-gid (sxpath "/g:googleId/text()" gns))

(: sx-price SXPath)
(define sx-price (sxpath "/g:inventories/g:inventory[@channel='online']/g:price/text()" gns))

(: sx-brand SXPath)
(define sx-brand (sxpath "/g:brand/text()" gns))

(: parse-search-result (Sxml -> (Option SearchResult)))
(define (parse-search-result prod)
  (let ((gtin (extract-text (sx-gtin prod)))
	(title (extract-text (sx-title prod)))
	(name (extract-text (sx-name prod)))
	(account (extract-integer (sx-account prod)))
	(link (extract-text (sx-link prod)))
	(gid (extract-text (sx-gid prod)))
	(brand (extract-text (sx-brand prod)))
	(price (extract-text (sx-price prod))))
    (if (and account gtin)
	(SearchResult name account title gtin gid link brand price)
	#f)))

(: product-search (String QParams -> (Listof SearchResult)))
(define (product-search query restrictions)
  (let ((url (make-query-uri (append (list (query-q-param query) access-key-param)
				     restrictions))))
    ;; (pretty-print (uri->string url))
    (with-handlers ([exn:fail? 
		     (lambda (ex) 
		       (pretty-print ex)
		       '())])
      (let ((conn (http-invoke 'GET url search-request-headers #f)))
	;;(pretty-print conn)
	(if (http-successful? conn)       
	    (let ((page (xml->sxml (HTTPConnection-in conn) '())))
	      (http-close-connection conn)
	      ;;(pretty-print page)
	      (let ((prods (sx-prods page)))
		;;(pretty-print prods)
		(if (andmap list? prods)
		    (filter SearchResult? (map parse-search-result prods))
		    '())))
	    (begin
	      (http-close-connection conn)
	      '()))))))
