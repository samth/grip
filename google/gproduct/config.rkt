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
  (list (cons "country" "US")
	;;(cons "brand "")
	(cons "alt" "atom")
	(cons "maxResults" "1000")))

;; "&restrictBy=condition=new,accountId=10048|7933&alt=atom"))

