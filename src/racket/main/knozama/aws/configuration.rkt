#lang racket

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Simple storage of configuration values.
;; we could have multiple configuration maps 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide aws-credentials-path
	 s3-host
	 a2s-host
	 a2s-ns
	 a2s-nss)

;; path to configuration file containing AWS credentials
(define aws-credentials-path "/home/rpr/awsaccount.txt")

;; host for Amazon Associate Services
(define a2s-host "webservices.amazon.com")

;; S3 storage host
(define s3-host "s3.amazonaws.com")

;; Amazon Associate Services namespace
(define a2s-namespace "http://webservices.amazon.com/AWSECommerceService/2008-06-26")

(define a2s-ns
  (cons 'a2s a2s-namespace))

(define a2s-nss
  (list a2s-ns))

