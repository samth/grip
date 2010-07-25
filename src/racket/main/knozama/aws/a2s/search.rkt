#lang racket

(require knozama/aws/credentials
	 knozama/aws/configuration
	 knozama/web/uri
	 knozama/web/uri/url/param
	 (only-in net/uri-codec
		  uri-encode))

(define ecs-parms
  (lambda (creds)
    `(("Service"        . "AWSECommerceService")
      ("Version"        . "2010-11-01")         
      ("AssociateTag"   . ,(aws-credentials-associate-tag creds))
      ("AWSAccessKeyId" . ,(aws-credentials-access-key creds)))))

(define search-parms
  '(("Operation"     . "ItemSearch")
    ;; ("SearchIndex"   . "Books")
    ("ResponseGroup" . "SalesRank,Small,EditorialReview,Images")))

(define keyword-search
  (lambda (creds index-sym words) 
    (let ((parms (append search-parms (ecs-parms creds)
		       `(("Keywords" . ,(uri-encode words))
			 ,(case index-sym
			    ((KINDLE)
			     '("SearchIndex" . "KindleStore"))
			    (else '("SearchIndex" . "Books")))))))
      (let ((uri (uri "http" #f a2s-host #f "/onca/xml" (parms->query parms) "")))
	uri))))






;; (let-values (((hdrs hip) (http-invoke 'GET uri `(,a2s-host-header) #f)))
;;   (let ((tip (transcoded-port hip (make-transcoder (utf-8-codec)))))
;;     (call/cc (lambda (k)
;; 	       (with-exception-handler
;; 		(lambda (ec)
;; 		  (pretty-print "ERROR in item search.")
;; 		  (pretty-print uri)
;; 		  (pretty-print hdrs)				   
;; 		  (close-port tip)
;; 		  (k '()))
;; 		(lambda ()
;; 		  (let ((results (xml->sxml tip '())))
;; 		    (close-port tip)
;; 		    results)))))))))))


