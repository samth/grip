#lang typed/racket/base

(require
 racket/pretty
 (only-in (planet knozama/webkit:1/web/uri/url/param)
	  parse-parms)
 (only-in (planet knozama/webkit:1/web/uri)
	  Uri Uri-query
          make-uri parse-uri))

;; (: make-db-uri (String -> Uri)) 
;; (define (make-db-uri action )
;;   (make-uri "https" "sdb.amazon.aws.com"

;; (: create-domain (String -> DBResult))
;; (define (create-domain domain)
;;   (

(define (test)
  (let ((uri (assert (parse-uri (string-append "https://sdb.amazon.com"
					     "?DomainName=MyDomain"
					     "&ItemName=Item123"
					     "&Attribute.1.Name=Color&Attribute.1.Value=Blue"
					     "&Attribute.2.Name=Size&Attribute.2.Value=Med"
					     "&Attribute.3.Name=Price&Attribute.3.Value=0014.99"
					     "&Version=2009-04-15"
					     "&Timestamp=2010-01-25T15%3A01%3A28-07%3A00"
					     "&SignatureVersion=2"
					     "&SignatureMethod=HmacSHA256"
					     "&AWSAccessKeyId=KEY")))))
    (pretty-print (parse-parms (assert (Uri-query uri))))))


;; https://sdb.amazonaws.com/
;; ?Action=CreateDomain
;; &AWSAccessKeyId=[valid access key id]
;; &DomainName=MyDomain
;; &SignatureVersion=2
;; &SignatureMethod=HmacSHA256
;; &Timestamp=2010-01-25T15%3A01%3A28-07%3A00
;; &Version=2009-04-15
;; &Signature=[valid signature]
