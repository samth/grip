#lang typed/racket/base

(provide
 parse-session-response)

(require 
 (only-in (planet knozama/xml:1/sxml)
	  Sxml SXPath sxpath extract-text extract-integer)
 (only-in (planet knozama/aws:1/credential)
	  AwsCredential AwsCredential-set-session! SessionCredential current-aws-credential)
 (only-in "error.rkt"
	  STSError)
 (only-in "config.rkt"
	  sts-ns))

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
	      (expiration (extract-text (sx-session-expiration result))))
	  (let ((session (SessionCredential access secret token expiration)))	    
	    (struct-copy AwsCredential (current-aws-credential) [session session])))
	(STSError))))
 
