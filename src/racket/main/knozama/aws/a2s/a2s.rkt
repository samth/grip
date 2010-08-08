#lang racket

(provide keyword-search browse-node
	 item-lookup similarity-lookup	 
	 cart-create cart-clear cart-get cart-add)

(require
 (only-in (planet lizorkin/ssax:2:0/ssax)
	  ssax:xml->sxml)
 (only-in (planet lizorkin/sxml:2:1/sxml)
	  sxpath sxml:text srl:sxml->html)
 (only-in knozama/std/prelude
	  fx1+)
 (only-in knozama/text/util
	  weave-string-separator)
 (only-in knozama/aws/configuration
	  a2s-host)
 (only-in knozama/aws/credentials
	  aws-credentials-associate-tag
	  aws-credentials-secret-key
	  aws-credentials-access-key)
 (only-in knozama/web/uri
	  make-uri uri->start-line-path-string
	  url-encode-string)
 (only-in knozama/web/http/http
	  http-invoke)
 (only-in knozama/type/date
	  current-time-iso-8601
	  current-time-rfc-2822)
 (only-in knozama/crypto/hash/sha256
	  hmac-sha256 hexstr)
 (only-in knozama/crypto/base64
	  base64-encode)
 (only-in knozama/aws/auth
	  aws-s3-auth-str
	  aws-s3-auth-mac)
 (only-in knozama/web/http/headers
	  host-header date-header)
 (only-in knozama/web/uri/url/param
	  parms->query)
 (only-in knozama/web/http/headers
	  host-header)
 (only-in knozama/aws/configuration
	  a2s-ns)
 (only-in knozama/web/http/resource
	  fetch-resource-xml))

(define empty-response
  (lambda () '()))

(define a2s-host-header (host-header a2s-host))

(define search-parms
  `(("Operation"     . "ItemSearch")
    ;; ("SearchIndex"   . "Books")
    ("ResponseGroup" . ,(url-encode-string "SalesRank,Small,EditorialReview,Images" #f))))

(define itemlookup-parms
  '(("Operation"   . "ItemLookup")))

(define browse-parms
  '(("Operation" . "BrowseNodeLookup")
    ("ResponseGroup" . "BrowseNodeInfo")))

(define ecs-parms
  (lambda (creds)
    `(("Service"        . "AWSECommerceService")
      ("Version"        . "2010-11-01")         
      ("AssociateTag"   . ,(aws-credentials-associate-tag creds))
      ("AWSAccessKeyId" . ,(aws-credentials-access-key creds)))))

(define browse-node
  (lambda (creds node-id)
    (let ((parms (append browse-parms (ecs-parms creds)
		       `(("BrowseNodeId" . ,(number->string node-id))
			 ("Timestamp" . ,(url-encode-string (current-time-iso-8601) #f))))))
      (let* ((sig (sign-request creds "GET" a2s-host "/onca/xml" parms))
	   (parms (cons (cons "Signature" (url-encode-string sig #f)) parms))
	   (uri (make-uri "http" #f a2s-host #f "/onca/xml" (parms->query parms) "")))
	(let-values (((hdrs ip) (http-invoke 'GET uri `(,a2s-host-header) #f)))
	  (call-with-exception-handler
	   (lambda (ec)
	     (pretty-print "ERROR in browse.")
	     (pretty-print uri)
	     (pretty-print hdrs)
	     (close-input-port ip)
	     empty-response)
	   (lambda ()
	     (let ((results (ssax:xml->sxml ip '())))
	       (close-input-port ip)
	       results))))))))

(define keyword-search
  (lambda (creds index-sym words) 
    (let ((parms (append search-parms (ecs-parms creds)
		       `(("Keywords" . ,(url-encode-string words #f))
			 ("Timestamp" . ,(url-encode-string (current-time-iso-8601) #f))
			 ,(case index-sym
			    ((KINDLE)
			     '("SearchIndex" . "KindleStore"))
			    (else '("SearchIndex" . "Books")))))))
      (let* ((sig (sign-request creds "GET" a2s-host "/onca/xml" parms))
	   (parms (cons (cons "Signature" (url-encode-string sig #f)) parms))
	   (uri (make-uri "http" #f a2s-host #f "/onca/xml" (parms->query parms) "")))
	(let-values (((hdrs ip) (http-invoke 'GET uri `(,a2s-host-header) #f)))
	  (call-with-exception-handler
	   (lambda (ec)
	     (pretty-print "ERROR in item search.")
	     (pretty-print uri)
	     (pretty-print hdrs)
	     (close-input-port ip)
	     empty-response)
	   (lambda ()
	     (let ((results (ssax:xml->sxml ip '())))
	       (close-input-port ip)
	       results))))))))

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
    (let ((parms (weave-string-separator "&" (sort (map (lambda (pair)
							(string-append (car pair) "=" (cdr pair)))
						      params) string<?))))
      (let ((auth-str (weave-string-separator "\n" (list action host path parms))))
	(base64-encode (hmac-sha256 (aws-credentials-secret-key creds) auth-str))))))

(define item-lookup
  (lambda (creds asin)
    (let ((parms (append itemlookup-parms (ecs-parms creds)
		       `(("IdType" . "ASIN")
			 ("ItemId" . ,asin)
			 ("ResponseGroup" . ,(url-encode-string "SalesRank,Small,ItemAttributes,EditorialReview,Images,Reviews,Offers,Similarities" #f))
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

