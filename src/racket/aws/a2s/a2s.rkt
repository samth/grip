#lang typed/racket/base

(provide 
 a2s-invoke
 credentials 
 ;;fetch-parse 
 browse-node service-parms
 empty-response
 sign-request
 item-lookup similarity-lookup)

(require/typed racket/base
	       (error-display-handler (-> (String Any -> Any))))

(require/typed 
 (planet lizorkin/ssax:2:0/ssax)
 (ssax:xml->sxml (Input-Port (Listof String) -> (Listof Any))))

(require
 racket/pretty
 (only-in (planet lizorkin/sxml:2:1/sxml)
	  sxpath sxml:text srl:sxml->html)
 (only-in (planet knozama/common:1/text/util)
	  weave-string-separator)
 (only-in (planet knozama/webkit:1/web/uri)
	  make-uri uri->start-line-path-string
	  url-encode-string)
 (only-in (planet knozama/webkit:1/web/http/http11)
	  http-invoke)
 (only-in (planet knozama/common:1/type/date)
	  current-time-iso-8601
	  current-time-rfc-2822)
 (only-in (planet knozama/webkit:1/crypto/hash/sha256)
	  hmac-sha256 hexstr)
 (only-in (planet knozama/webkit:1/crypto/base64)
	  base64-encode)
 (only-in (planet knozama/webkit:1/web/http/headers)
	  Headers Header host-header date-header)
 (only-in (planet knozama/webkit:1/web/uri/url/param)
	  parms->query)
 (only-in (planet knozama/webkit:1/web/http/headers)
	  host-header)
 (only-in (planet knozama/webkit:1/web/http/resource)
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

(: empty-response (List Symbol))
(define empty-response '(*TOP*))

(: a2s-host-header String)
(define a2s-host-header (host-header a2s-host))

(: itemlookup-parms Headers)
(define itemlookup-parms
  '(("Operation"   . "ItemLookup")))

(: browse-parms Headers)
(define browse-parms
  '(("Operation"     . "BrowseNodeLookup")
    ("ResponseGroup" . "BrowseNodeInfo")))

(: service-parms Headers)
(define service-parms
  `(("Service"        . "AWSECommerceService")
    ("Version"        . "2010-11-01")))

(: core-parms (-> Headers))
(define (core-parms)
  (cons `("Timestamp" . ,(url-encode-string (current-time-iso-8601) #f))
	(cons `("AWSAccessKeyId" . ,(aws-credential-access-key (credentials)))
	      service-parms)))

(: browse-node (Integer -> (Listof Any)))
(define (browse-node node-id)
  (let ((parms (append `(("BrowseNodeId" . ,(number->string node-id))) browse-parms)))
    (a2s-invoke parms)))


(: similarity-lookup ((Listof String) -> (Listof Any)))
(define (similarity-lookup asins)
  (let ((parms `(("Operation" . "SimilarityLookup")
	       ("IdType" . "ASIN")
	       ("ItemId" . ,(weave-string-separator "," asins))
	       ("ResponseGroup" . ,(url-encode-string "SalesRank,ItemAttributes,Images,EditorialReview" #f)))))
    (displayln "SIMILARITY")
    (a2s-invoke parms)))

(: item-lookup (String -> (Listof Any)))
(define (item-lookup asin)
  (let ((parms (append itemlookup-parms
		     `(("IdType" . "ASIN")
		       ("ItemId" . ,asin)
		       ("ResponseGroup" . ,(url-encode-string "BrowseNodes" #f))))))
    ;;  ("ResponseGroup" . ,(url-encode-string "Small,Reviews" #f))))))
    ;;("ResponseGroup" . ,(url-encode-string "SalesRank,Small,ItemAttributes,EditorialReview,Images,Reviews,Offers,Similarities" #f))
    ;; (displayln "ITEM LOOKUP")
    (a2s-invoke parms)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; A2S required sorted param string ready for signing.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(: params->string (Headers -> String))
(define (params->string params)
  (define str-sort (inst sort String String))
  (weave-string-separator "&" (str-sort (map (lambda: ((pair : Header))
					       (string-append (car pair) "=" (cdr pair)))
					     params) string<?)))

(: sign-request (String Headers -> String))
(define (sign-request action params)  
  (let* ((param-str (params->string params))
       (auth-str (weave-string-separator "\n" (list action 
						    a2s-host 
						    a2s-path 
						    param-str)))
       (sig (url-encode-string (base64-encode 
				(hmac-sha256 
				 (aws-credential-secret-key (credentials))
				 auth-str)) #f)))
    (string-append "Signature=" sig "&" param-str)))


;; Generic call procedure to the REST A2S API 
(: a2s-invoke (Headers -> (Listof Any)))
(define (a2s-invoke params)
  (let* ((parm-str (sign-request "GET" (append (core-parms) params)))
       (uri (make-uri "http" #f a2s-host #f a2s-path parm-str "")))	
    (if uri
       (let-values (((hdrs ip) (http-invoke 'GET uri '() #f)))
	 (with-handlers [(exn:fail? (lambda (ex) 
				      ((error-display-handler) "ERROR in a2s invocation." ex)
				      (displayln ex) 
				      (close-input-port ip)
				      empty-response))]
	   (let ((results (ssax:xml->sxml ip '())))
	     (close-input-port ip)
	     results)))
       empty-response)))
