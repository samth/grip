#lang racket

(provide) ;;keyword-search)

;; (require knozama/aws/credentials
;; 	 knozama/aws/configuration
;; 	 knozama/web/uri
;; 	 knozama/web/uri/url/param
;; 	 (only-in net/uri-codec
;; 		  uri-encode)
;; 	 (only-in knozama/web/http/http
;; 		  parse-http-response-line
;; 		  response-line-code
;; 		  http-invoke)
;; 	 (only-in knozama/aws/a2s/a2s
;; 		  a2s-host-header))


;; (define ecs-parms
;;   (lambda (creds)
;;     `(("Service"        . "AWSECommerceService")
;;       ("Version"        . "2010-11-01")         
;;       ("AssociateTag"   . ,(aws-credentials-associate-tag creds))
;;       ("AWSAccessKeyId" . ,(aws-credentials-access-key creds)))))

;; (define search-parms
;;   '(("Operation"     . "ItemSearch")
;;     ;; ("SearchIndex"   . "Books")
;;     ("ResponseGroup" . "SalesRank,Small,EditorialReview,Images")))

;; (define keyword-search
;;   (lambda (creds index-sym words) 
;;     (let ((parms (append search-parms (ecs-parms creds)
;; 		       `(("Keywords" . ,(uri-encode words))
;; 			 ,(case index-sym
;; 			    ((KINDLE)
;; 			     '("SearchIndex" . "KindleStore"))
;; 			    (else '("SearchIndex" . "Books")))))))
;;       (let ((uri (uri "http" (authority #f a2s-host #f) "/onca/xml" (parms->query parms) "")))       
;; 	(let-values (((hdrs ip) (http-invoke 'GET uri `(,a2s-host-header) #f)))
;; 	  (let ((http-resp (parse-http-response-line (car hdrs))))
;; 	    (if (string=? (response-line-code http-resp) "200")
;; 	       (call-with-exception-handler
;; 		(lambda (e)
;; 		  (pretty-print "ERROR in item search.")
;; 		  (pretty-print uri)
;; 		  (pretty-print hdrs)				   
;; 		  (close-input-port ip)
;; 		  '())
;; 		(lambda ()
;; 		  (let ((results (xml->sxml ip '())))
;; 		    (close-input-port ip)
;; 		    results)))
;; 	       (call-with-exception-handler
;; 		(lambda (e)
;; 		  (pretty-print "ERROR in item search.")
;; 		  (pretty-print uri)
;; 		  (pretty-print hdrs)				   
;; 		  (close-input-port ip)
;; 		  '())
;; 		(lambda ()
;; 		  (display "Failed with error: ")(display http-resp)(newline)
;; 		  (let ((results (xml->sxml ip '())))
;; 		    (close-input-port ip)
;; 		    results))))))))))
		  
		 


