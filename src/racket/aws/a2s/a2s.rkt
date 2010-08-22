#lang racket/base

(provide 
 a2s-invoke
 credentials 
 ;;fetch-parse 
 browse-node service-parms
 empty-response
 sign-request
 item-lookup similarity-lookup)

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
	  credential-path
	  a2s-ns a2s-host a2s-path)
 (only-in "../credential.rkt"
	  load-credential
	  aws-credential-associate-tag
	  aws-credential-secret-key
	  aws-credential-access-key)
 (only-in "../auth.rkt"
	  aws-s3-auth-str
	  aws-s3-auth-mac))

(define credentials (make-parameter (load-credential credential-path)))

(define empty-response 
  '(*TOP*))

(define a2s-host-header (host-header a2s-host))

(define itemlookup-parms
  '(("Operation"   . "ItemLookup")))

(define browse-parms
  '(("Operation"     . "BrowseNodeLookup")
    ("ResponseGroup" . "BrowseNodeInfo")))

(define service-parms
  `(("Service"        . "AWSECommerceService")
    ("Version"        . "2010-11-01")))

(define core-parms
  (lambda ()
    (cons `("Timestamp" . ,(url-encode-string (current-time-iso-8601) #f))
	  (cons `("AWSAccessKeyId" . ,(aws-credential-access-key (credentials)))
		service-parms))))

(define browse-node
  (lambda (node-id)
    (let ((parms (append `(("BrowseNodeId" . ,(number->string node-id))) browse-parms)))
      (a2s-invoke parms))))


(define similarity-lookup  
  (lambda (asins)
    (let ((parms `(("Operation" . "SimilarityLookup")
		   ("IdType" . "ASIN")
		   ("ItemId" . ,(weave-string-separator "," asins))
		   ("ResponseGroup" . ,(url-encode-string "SalesRank,ItemAttributes,Images,EditorialReview" #f)))))
      (displayln "SIMILARITY")
      (a2s-invoke parms))))

(define item-lookup
  (lambda (asin)
    (let ((parms (append itemlookup-parms
			 `(("IdType" . "ASIN")
			   ("ItemId" . ,asin)
			   ("ResponseGroup" . ,(url-encode-string "Small,ItemAttributes" #f))))))
      ;;("ResponseGroup" . ,(url-encode-string "SalesRank,Small,ItemAttributes,EditorialReview,Images,Reviews,Offers,Similarities" #f))
      (displayln "ITEM LOOKUP")
      (a2s-invoke parms))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; A2S required sorted param string ready for signing.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define params->string
  (lambda (params)
    (weave-string-separator "&" (sort (map (lambda (pair)
					     (string-append (car pair) "=" (cdr pair)))
					   params) string<?))))


(define sign-request 
  (lambda (action params)    
    (let* ((param-str (params->string params))
	   (auth-str (weave-string-separator "\n" (list action 
							a2s-host 
							a2s-path 
							param-str)))
	   (sig (url-encode-string (base64-encode 
				    (hmac-sha256 
				     (aws-credential-secret-key (credentials))
				     auth-str)) #f)))
      (string-append "Signature=" sig "&" param-str))))


;; Generic call procedure to the REST A2S API 
(define a2s-invoke
  (lambda (params)
    (let* ((parm-str (sign-request "GET" (append (core-parms) params)))
	   (uri (make-uri "http" #f a2s-host #f a2s-path parm-str "")))	
      (let-values (((hdrs ip) (http-invoke 'GET uri '() #f)))
	(call-with-exception-handler
	 (lambda (e)
	   ((error-display-handler) "ERROR in a2s invocation." e)
	   (displayln e)
	   (close-input-port ip)
	   empty-response)
	 (lambda ()
	   (let ((results (ssax:xml->sxml ip '())))
	     (close-input-port ip)
	     results)))))))

