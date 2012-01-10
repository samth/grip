#lang typed/racket/base

(provide
 gproduct-host gproduct-path std-query-params)

(: gproduct-host String)
(define gproduct-host "www.googleapis.com")

(: gproduct-path String)
(define gproduct-path "/shopping/search/v1/public/products")

(: std-query-params (Listof (Pairof String String)))
(define std-query-params   
  (list (cons "fields" "entry/s:product(s:author,s:gtin,s:title,s:link)&alt=atom")
	(cons "country" "US")
	;;(cons "brand "")
	(cons "alt" "atom")))

