#lang typed/racket/base

(provide
 gproduct-host gproduct-path std-query-params gproduct-nss)

(: gproduct-nss String)
(define gproduct-nss "http://www.google.com/shopping/api/schemas/2010")

(: gproduct-host String)
(define gproduct-host "www.googleapis.com")

(: gproduct-path String)
(define gproduct-path "/shopping/search/v1/public/products")

(: std-query-params (Listof (Pairof String String)))
(define std-query-params   
  (list (cons "fields" (string-append 
			"entry/s:product(s:author,s:gtin,s:title,s:link)"
			"&restrictBy=condition=new&alt=atom"))
	(cons "country" "US")
	;;(cons "brand "")
	(cons "alt" "atom")))

