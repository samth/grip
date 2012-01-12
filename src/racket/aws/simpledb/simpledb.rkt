#lang typed/racket/base

(require
 racket/pretty
 (only-in (planet knozama/aws:1/auth)
	  sdb-auth-str sdb-auth-mac-encode)
 (only-in (planet knozama/aws:1/credential)
	  Aws-Credential-secret-key Aws-Credential-access-key current-aws-credential)
 (only-in (planet knozama/webkit:1/web/uri/url/param)
	  params->query parse-params)
 (only-in (planet knozama/webkit:1/crypto/base64)
	  base64-encode)
 (only-in (planet knozama/webkit:1/crypto/hmac)
	  hmac-sha256)
 (only-in (planet knozama/webkit:1/web/uri)
	  Uri Uri-query make-uri parse-uri
	  url-encode-string uri->string)
 (only-in (planet knozama/common:1/type/date)
	  current-time-iso-8601)
 (only-in (planet knozama/webkit:1/web/http/http11)
	  HTTPConnection-in http-successful? http-close-connection http-invoke)
 (only-in (planet knozama/webkit:1/web/http/header)
          make-header-string)
 (only-in (planet knozama/xml:1/sxml)
	  Sxml SXPath html->sxml xml->sxml extract-text extract-integer)
 (only-in "../configuration.rkt"
	  sdb-std-params))
 
;; (: make-db-uri (String -> Uri)) 
;; (define (make-db-uri action )
;;   (make-uri "https" "sdb.amazon.aws.com"

;; (: create-domain (String -> DBResult))
;; (define (create-domain domain)
;;   (

(: request-signature (String -> String))
(define (request-signature signee)
  (url-encode-string (base64-encode (hmac-sha256 (Aws-Credential-secret-key (current-aws-credential)) signee)) #f))


(define request-headers  (list 
		     ;; (make-header-string "User-Agent" "Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/535.11 (KHTML, like Gecko) Chrome/17.0.963.2 Safari/535.11")
		     (make-header-string "User-Agent" "Googlebot/2.1 (+http://www.google.com/bot.html)")
		     (make-header-string "Accept" "text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8")
		     (make-header-string "Accept-Charset" "ISO-8859-1,utf-8;q=0.7,*;q=0.3")
		     ;;(make-header-string "Accept-Encoding" "gzip,deflate,sdch")
		     (make-header-string "Accept-Language" "en-US,en;q=0.8")
		     (make-header-string "Cache-Control" "max-age=0")
		     (make-header-string "Connection" "keep-alive")))

(: test-create (-> String))
(define (test-create)
  (let ((qparams (append (sdb-std-params (Aws-Credential-access-key (current-aws-credential)))
		       (list (cons "Action" "CreateDomain")
			     (cons "DomainName" "TestDomain")
			     (cons "Timestamp" (url-encode-string (current-time-iso-8601) #f))))))
    (pretty-print qparams)
    (let ((sig-para (cons "Signature"
			(request-signature (sdb-auth-str "GET" "/" qparams)))))
      (let ((url (make-uri "https" #f "sdb.amazonaws.com" 443 "/" 
			 (params->query (cons sig-para qparams)) "")))
	(pretty-print (uri->string url))
	(with-handlers ([exn:fail? 
			 (lambda (ex) #f)])
	  (let ((conn (http-invoke 'GET url request-headers #f)))
	    (pretty-print conn)
	    ;; (if (http-successful? conn)
	    (let ((page (xml->sxml (HTTPConnection-in conn) '())))
	      (http-close-connection conn)
	      (pretty-print page)))))))
  "Done")
    
;; (let ((uri (assert (parse-uri (string-append "https://sdb.amazonaws.com/"
;; 					     "?Action=CreateDomain"
;; 					     "&DomainName=TestDomain"
;; 					     "&Timestamp=" 
;; 					     (url-encode-string (current-time-iso-8601) #f))))))
  
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

