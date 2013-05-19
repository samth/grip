#lang typed/racket/base

(provide
 gproduct-host gproduct-path std-query-params gproduct-nss)

(require
 (only-in net/uri/url/url
	  QParams QParam))

(: gproduct-nss String)
(define gproduct-nss "http://www.google.com/shopping/api/schemas/2010")

(: gproduct-host String)
(define gproduct-host "www.googleapis.com")

(: gproduct-path String)
(define gproduct-path "/shopping/search/v1/public/products")

(: std-query-params QParams)
(define std-query-params   
  (list (QParam "country" "US")
	;;(cons "brand "")
	(QParam "alt" "atom")
	(QParam "maxResults" "1000")))

;; "&restrictBy=condition=new,accountId=10048|7933&alt=atom"))

