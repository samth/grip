#lang typed/racket/base

(require 
 (only-in (planet knozama/aws:1/credential)
	  current-aws-credential
	  init-aws-credential)
 (only-in (planet knozama/aws:1/s3/s3)
	  list-buckets))

(: credential-path Path)
(define credential-path (string->path "/home/ray/awsaccount.txt"))

(init-aws-credential credential-path)

(: my-buckets (-> (Listof Any)))
(define (my-buckets)
  (list-buckets "/"))
