#lang typed/racket/base

(provide 
 list-buckets)

(require
 racket/pretty
 (only-in (planet rpr/format:1/xml/sxml)
	  Sxml SXPath 
	  sxpath xml->sxml select-single-node-text)
 (only-in "types.rkt" 
	  Owner Bucket Buckets Objects Object)
 (only-in "configuration.rkt"
	  nss)
 (only-in "invoke.rkt"
	  make-base-uri s3-invoke
	  S3Response S3Response-sxml))

(: list-buckets (-> Buckets))
(define (list-buckets)

  (: parse-owner (Sxml -> Owner))
  (define (parse-owner sxml)
    (define sx-id (select-single-node-text "/s3:Owner/s3:ID" nss))
    (define sx-name (select-single-node-text "/s3:Owner/s3:DisplayName" nss))
    (let ((id (sx-id sxml))
	  (name (sx-name sxml)))
      (Owner id name)))

  (: parse-bucket (Sxml -> Bucket))
  (define (parse-bucket sxml)
    (define sx-name (select-single-node-text "/s3:Name" nss))
    (define sx-create (select-single-node-text "/s3:CreationDate" nss))
    (let ((name (sx-name sxml))
	  (create (sx-create sxml)))
      (Bucket name create)))

  (: parse-buckets (Sxml -> (Listof Bucket)))
  (define (parse-buckets sxml)
    (: sx-buckets SXPath)
    (define sx-buckets (sxpath "/s3:Buckets/s3:Bucket" nss))
    (let ((bs (sx-buckets sxml)))
      (if (andmap list? bs)
	  (map parse-bucket bs)
	  '())))

  (: parse-response (Sxml -> Buckets))
  (define (parse-response sxml)
    (: sx-resp SXPath)
    (define sx-resp (sxpath "/s3:ListAllMyBucketsResult" nss))
    (let ((resp (sx-resp sxml)))
      (if resp
	  (begin
	    (let ((owner (parse-owner resp))
		  (buckets (parse-buckets resp)))
	      (Buckets owner buckets)))
	  (error "S3 call failed"))))

  (let ((resp (s3-invoke 'GET #f "/" #f '() #f)))
    (parse-response (S3Response-sxml resp))))

(: create-bucket (String -> S3Response))
(define (create-bucket bucket)
  (s3-invoke 'PUT bucket "/" #f '() #f))

(: delete-bucket (String -> S3Response))
(define (delete-bucket bucket)
  (s3-invoke 'DELETE bucket "/" #f '() #f))
