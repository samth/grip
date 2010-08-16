#lang racket/base

(provide fetch-parse browse-node service-parms
	 make-signer a2s-uri
	 empty-response
	 sign-request
	 item-lookup similarity-lookup	 
	 cart-create cart-clear cart-get cart-add)

(require
 racket/pretty
 (only-in (planet lizorkin/ssax:2:0/ssax)
	  ssax:xml->sxml)
 (only-in (planet lizorkin/sxml:2:1/sxml)
	  sxpath sxml:text srl:sxml->html)
 (only-in (planet knozama/common:1:0/text/util)
	  weave-string-separator)
 (only-in (planet knozama/common:1:0/std/prelude)
	  fx1+)
 (only-in (planet knozama/webkit:1:0/web/uri)
	  make-uri uri->start-line-path-string
	  url-encode-string)
 (only-in (planet knozama/webkit:1:0/web/http/http11)
	  http-invoke)
 (only-in (planet knozama/common:1:0/type/date)
	  current-time-iso-8601
	  current-time-rfc-2822)
 (only-in (planet knozama/webkit:1:0/crypto/hash/sha256)
	  hmac-sha256 hexstr)
 (only-in (planet knozama/webkit:1:0/crypto/base64)
	  base64-encode)
 (only-in (planet knozama/webkit:1:0/web/http/headers)
	  host-header date-header)
 (only-in (planet knozama/webkit:1:0/web/uri/url/param)
	  parms->query)
 (only-in (planet knozama/webkit:1:0/web/http/headers)
	  host-header)
 (only-in (planet knozama/webkit:1:0/web/http/resource)
	  fetch-resource-xml)
 (only-in "../configuration.rkt"
	  a2s-ns a2s-host a2s-path)
 (only-in "../credential.rkt"
	  aws-credential-associate-tag
	  aws-credential-secret-key
	  aws-credential-access-key)
 (only-in "../auth.rkt"
	  aws-s3-auth-str
	  aws-s3-auth-mac))

(define empty-response
  (lambda () '(*TOP*)))

(define a2s-host-header (host-header a2s-host))

(define itemlookup-parms
  '(("Operation"   . "ItemLookup")))

(define browse-parms
  '(("Operation"     . "BrowseNodeLookup")
    ("ResponseGroup" . "BrowseNodeInfo")))

(define service-parms
  `(("Service"        . "AWSECommerceService")
    ("Version"        . "2010-11-01")))

(define ecs-parms
  (lambda (creds)
    `(("Service"        . "AWSECommerceService")
      ("Version"        . "2010-11-01")         
      ("AssociateTag"   . ,(aws-credential-associate-tag creds))
      ("AWSAccessKeyId" . ,(aws-credential-access-key creds)))))

(define fetch-parse
  (lambda (uri headers error)
    (let-values (((hdrs ip) (http-invoke 'GET uri headers #f)))
      (call-with-exception-handler
       (lambda (e)
	 ((error-display-handler)  "ERROR in browse." e)
	 (pretty-print uri)
	 (pretty-print hdrs)
	 (close-input-port ip)
	 (if error
	    (error e)
	    '(*TOP*)))
       (lambda ()
	 (let ((results (ssax:xml->sxml ip '())))
	   (close-input-port ip)
	   (displayln results)
	   results))))))

(define browse-node
  (lambda (creds node-id)
    (let ((parms (append browse-parms (ecs-parms creds)
		       `(("BrowseNodeId" . ,(number->string node-id))
			 ("Timestamp" . ,(url-encode-string (current-time-iso-8601) #f))))))
      (let* ((sig (sign-request creds "GET" a2s-host "/onca/xml" parms))
	   (parms (cons (cons "Signature" (url-encode-string sig #f)) parms))
	   (uri (make-uri "http" #f a2s-host #f "/onca/xml" (parms->query parms) "")))
	
	(fetch-parse uri `(,a2s-host-header) #f)))))


(define similarity-lookup  
  (lambda (creds asins)
    (let ((parms (append itemlookup-parms (ecs-parms creds)
		       `(("IdType" . "ASIN")
			 ("ItemId" . ,(weave-string-separator "," asins))
			 ("ResponseGroup" . "SalesRank,ItemAttributes,Images,EditorialReview")))))
      ;;(pretty-print parms)
      (let ((uri (make-uri "http" #f a2s-host #f "/onca/xml" (parms->query parms) "")))
	(fetch-resource-xml uri `(,a2s-host-header) empty-response)))))


(define sign-request 
  (lambda (creds action host path params)
    (displayln action)
    (displayln host)
    (displayln path)
    (displayln params)
    (let ((parms (weave-string-separator "&" (sort (map (lambda (pair)
							(string-append (car pair) "=" (cdr pair)))
						      params) string<?))))
      (let ((auth-str (weave-string-separator "\n" (list action host path parms))))
	(displayln "====================")
	(displayln auth-str)
	(displayln "====================")
	(base64-encode (hmac-sha256 (aws-credential-secret-key creds) auth-str))))))

(define make-signer
  (lambda (creds)
    (lambda (action parms)
      (cons (cons "Signature" 
		   (url-encode-string (sign-request creds action a2s-host a2s-path parms) #f))
	     parms))))

(define a2s-uri
  (lambda (parms)
    (make-uri "http" #f a2s-host #f a2s-path (parms->query parms) "")))


(define item-lookup
  (lambda (creds asin)
    (let ((parms (append itemlookup-parms (ecs-parms creds)
		       `(("IdType" . "ASIN")
			 ("ItemId" . ,asin)
			 ("ResponseGroup" . ,(url-encode-string "Small,ItemAttributes" #f))
			 ;;("ResponseGroup" . ,(url-encode-string "SalesRank,Small,ItemAttributes,EditorialReview,Images,Reviews,Offers,Similarities" #f))
			 ("Timestamp" . ,(url-encode-string (current-time-iso-8601) #f))))))
      (let ((sig (sign-request creds "GET" a2s-host "/onca/xml" parms)))
	(let* ((parms (cons (cons "Signature" (url-encode-string sig #f)) parms))
	     (uri (make-uri "http" #f a2s-host #f "/onca/xml" (parms->query parms) "")))
	  (let-values (((hdrs ip) (http-invoke 'GET uri `(,a2s-host-header) #f)))
	    (call-with-exception-handler		 
	     (lambda (e)
	       (pretty-print "ERROR in item lookup.")
	       (pretty-print uri)
	       (pretty-print hdrs))
	     (lambda ()
	       (let ((results (ssax:xml->sxml ip '())))
		 (close-input-port ip)
		 results)))))))))

;; Generic call procedure to the REST A2S API 
(define a2s-invoke
  (lambda (creds op-parms query-extra)
    (let ((parms (append op-parms (ecs-parms creds))))
      (let ((uri (make-uri "http" #f a2s-host #f "/onca/xml" 
			   (let ((parms (parms->query parms)))
			     (if query-extra
				 (string-append parms "&" query-extra)
				 parms))
			   "")))
	(let-values (((hdrs ip) (http-invoke 'GET uri `(,a2s-host-header) #f)))
	  (call-with-exception-handler
	   (lambda (e)
	     (displayln "Error in a2s invocation")
	     (displayln e)
	     (close-input-port ip)
	     empty-response)
	   (lambda ()
	     (let ((results (ssax:xml->sxml ip '())))
	       (close-input-port ip)
	       results))))))))

#|  Cart API |#

;; START HERE -- TEST CART CREATE

;;  (define cart-modify #f)

;; Given a line number and an alist of (asin . qty) create a cart line
;; Item.1.ASIN=123&Item.1.Quantity=10
(define make-cart-lines
  (lambda (entries)
    (let ((make-line (lambda (n line)
		       (let ((line-no (number->string n)))
			 (string-append "Item."   line-no ".ASIN=" (car line)
					"&Item."  line-no ".Quantity=" (number->string (cdr line)))))))
      (do ((accum '() (cons (make-line n (car entries)) accum))
	   (entries entries (cdr entries))
	   (n 1 (fx1+ n)))
	  ((null? entries) (weave-string-separator "&" (reverse accum)))))))


;; lines : alist of (item . qty)
(define cart-create 
  (lambda (creds items)
    (let ((parms `(("Operation" . "CartCreate"))))
      (a2s-invoke creds parms (make-cart-lines items)))))

(define cart-parms
  (lambda (operation cart-id hmac-encoded)
    `(("Operation" . ,operation)
      ("CartId"    . ,cart-id)
      ("HMAC"      . ,hmac-encoded))))

(define cart-clear
  (lambda (creds cart-id hmac-encoded)
    (a2s-invoke creds (cart-parms "CartClear" cart-id hmac-encoded) #f)))

(define cart-get
  (lambda (creds cart-id hmac-encoded)
    (a2s-invoke creds (cart-parms "CartGet" cart-id hmac-encoded) #f)))

(define cart-add
  (lambda (creds cart-id hmac-encoded items)
    (a2s-invoke creds 
		(cart-parms "CartAdd" cart-id hmac-encoded)
		(make-cart-lines items))))

