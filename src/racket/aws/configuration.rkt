#lang typed/racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Simple storage of configuration values.
;; we could have multiple configuration maps 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 
 s3-host
 a2s-host a2s-path a2s-ns a2s-nss
 sdb-host sdb-std-parms)

;; host for Amazon Associate Services
(define a2s-host "webservices.amazon.com")
(define a2s-path "/onca/xml")

;; S3 storage host
(define s3-host "s3.amazonaws.com")

;; Amazon Associate Services namespace
(define a2s-namespace "http://webservices.amazon.com/AWSECommerceService/2010-11-01")

(define a2s-ns
  (cons 'a2s a2s-namespace))

(define a2s-nss
  (list a2s-ns))

;;;;;;;;;;;;;;;;;;
;; AWS SimpleDB ;;
;;;;;;;;;;;;;;;;;;

(: sdb-host String)
(define sdb-host "sdb.amazonaws.com")

(: sdb-version String)
(define sdb-version "2009-04-15")

(: sdb-sig-version String)
(define sdb-sig-version "2")

(: sdb-sig-method String)
(define sdb-sig-method "HmacSHA256")

(: sdb-access-key String)
(define sdb-access-key "AWSAcessKeyId")

(: sdb-version-parm (Pairof String String))
(define sdb-version-parm (cons "Version" sdb-version))

(: sdb-sig-version-parm (Pairof String String))
(define sdb-sig-version-parm (cons "SignatureVersion" sdb-sig-version))

(: sdb-sig-method-parm (Pairof String String))
(define sdb-sig-method-parm (cons "SignatureMethod" sdb-sig-method))

(: sdb-access-key-parm (String -> (Pairof String String)))
(define (sdb-access-key-parm key)
  (cons sdb-access-key key))

(: sdb-std-parms (String -> (Listof (Pairof String String))))
(define (sdb-std-parms key)
  (list (sdb-access-key-parm key)
	sdb-version-parm 
	sdb-sig-version-parm 
	sdb-sig-method-parm))

