#lang typed/racket/base

(require
 racket/pretty
 (only-in (planet knozama/aws:1/auth)
	  sdb-auth-str sdb-auth-mac-encode)
 (only-in (planet knozama/aws:1/credential)
	  Aws-Credential-secret-key current-aws-credential)
 (only-in (planet knozama/webkit:1/web/uri/url/param)
	  parse-params)
 (only-in (planet knozama/webkit:1/crypto/base64)
	  base64-encode)
 (only-in (planet knozama/webkit:1/crypto/hmac)
	  hmac-sha256)
 (only-in (planet knozama/webkit:1/web/uri)
	  Uri Uri-query make-uri parse-uri
	  url-encode-string)
 (only-in (planet knozama/common:1/type/date)
	  current-time-iso-8601))

;; (: make-db-uri (String -> Uri)) 
;; (define (make-db-uri action )
;;   (make-uri "https" "sdb.amazon.aws.com"

;; (: create-domain (String -> DBResult))
;; (define (create-domain domain)
;;   (

(: request-signature (String -> String))
(define (request-signature signee)
  (url-encode-string (base64-encode (hmac-sha256 (Aws-Credential-secret-key (current-aws-credential)) signee)) #f))

(: test-create (-> String))
(define (test-create)
  (let ((uri (assert (parse-uri (string-append "https://sdb.amazonaws.com/"
					     "?Action=CreateDomain"
					     "&DomainName=TestDomain"
					     "&Timestamp=" 
					     (url-encode-string (current-time-iso-8601) #f))))))
    (let ((sig-para (cons "Signature" (request-signature (sdb-auth-str "GET" (parse-params (assert (Uri-query uri))))))))
      
  
(define (test)
  (pretty-print (test-create)))

;; https://sdb.amazonaws.com/
;; ?Action=CreateDomain
;; &AWSAccessKeyId=[valid access key id]
;; &DomainName=MyDomain
;; &SignatureVersion=2
;; &SignatureMethod=HmacSHA256
;; &Timestamp=2010-01-25T15%3A01%3A28-07%3A00
;; &Version=2009-04-15
;; &Signature=[valid signature]

;; https://sdb.amazonaws.com/
;; ?Action=CreateDomain
;; &AWSAccessKeyId=[valid access key id]
;; &DomainName=MyDomain
;; &SignatureVersion=2
;; &SignatureMethod=HmacSHA256
;; &Timestamp=2010-01-25T15%3A01%3A28-07%3A00
;; &Version=2009-04-15
;; &Signature=[valid signature]

