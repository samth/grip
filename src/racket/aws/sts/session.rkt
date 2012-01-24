#lang typed/racket/base

(provide
 parse-session-response)

(require 
 (only-in (planet knozama/xml:1/sxml)
	  Sxml SXPath sxpath extract-text extract-integer)
 (only-in (planet knozama/aws:1/credential)
	  Aws-Credential Aws-Credential-account-id Aws-Credential-associate-tag current-aws-credential)
 (only-in "error.rkt"
	  STSError)
 (only-in "config.rkt"
	  sts-ns))

;; (struct: SessionCredential ([token : String] 
;;    			       [access-key : String] 
;; 			       [secret-key : String]
;; 			       [expiration : String]))

(: mk-sxpath (String -> SXPath))
(define mk-sxpath
  (let ((sts-nss  `((sts . ,sts-ns))))
    (lambda (path)
      (sxpath path sts-nss))))

(define sx-session-result-creds (mk-sxpath "/sts:GetSessionTokenResponse/sts:GetSessionTokenResult/sts:Credentials"))
(define sx-session-token        (mk-sxpath "/sts:SessionToken/text()"))
(define sx-session-access-key   (mk-sxpath "/sts:AccessKeyId/text()"))
(define sx-session-secret-key   (mk-sxpath "/sts:SecretAccessKey/text()"))
(define sx-session-expiration   (mk-sxpath "/sts:Expiration/text()"))

(: parse-session-response (Sxml -> (U STSError Aws-Credential)))
(define (parse-session-response sxml)
  (let ((result (sx-session-result-creds sxml)))
    (if (pair? result)
       (let ((token (extract-text (sx-session-token result)))
	   (access (extract-text (sx-session-access-key result)))
	   (secret (extract-text (sx-session-secret-key result)))
	   (expiration (extract-text (sx-session-expiration result)))
	   (curr-cred (current-aws-credential)))
	 (Aws-Credential (Aws-Credential-account-id curr-cred)
			 access
			 secret
			 token
			 expiration
			 (Aws-Credential-associate-tag curr-cred)))
       (STSError))))
 
;; '(*TOP*
;;   (https://sts.amazonaws.com/doc/2011-06-15/:GetSessionTokenResponse
;;    (https://sts.amazonaws.com/doc/2011-06-15/:GetSessionTokenResult
;;     (https://sts.amazonaws.com/doc/2011-06-15/:Credentials
;;      (https://sts.amazonaws.com/doc/2011-06-15/:SessionToken
;;       "AQoDYXdzEDUasAEP1SvcIUrE63HAiQKeuuhZPTmyTpjoeSRn8xOW6GvGrwxag6zlFRqvjbS1TOwqZrorQACrn6yNO17SmXAX28lEFviPsJHtYeFQtmp/b7MYoSGfbHzVZHoI9ZAG1z5NANlQFBA42CCqdfk8kukDQ5uABCuJdPSvqNDCFV5weNMV/K9Y+LdV+XsGFCt1yT6qvl1ukNA5YShArGQsUZUPZCPtwuhvcXoHALX/PUF0AWQDZSCynfz4BA==")
;;      (https://sts.amazonaws.com/doc/2011-06-15/:SecretAccessKey
;;       "J4IXIPOQxNsLu1CzmBxn/x79gpbpF1viCEOYG3Gy")
;;      (https://sts.amazonaws.com/doc/2011-06-15/:Expiration
;;       "2012-01-24T21:04:02.889Z")
;;      (https://sts.amazonaws.com/doc/2011-06-15/:AccessKeyId
;;       "ASIAIXS65R34AX3XFF3Q")))
;;    (https://sts.amazonaws.com/doc/2011-06-15/:ResponseMetadata
;;     (https://sts.amazonaws.com/doc/2011-06-15/:RequestId
;;      "8fdbc91d-46c6-11e1-befd-43502c86b339"))))
