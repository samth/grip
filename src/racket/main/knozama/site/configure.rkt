#lang racket

(provide  aws-creds 
	  feedback-path
	  HOST PORT)

(require 
 (only-in knozama/web/server/static
	  www-root-set!)
 (only-in knozama/aws/credentials
	  load-credentials))

(define HOST "bpel.ray.na.odcorp.net")
(define PORT 8080)

(define feedback-path "/home/rpr")

(define aws-creds
  (load-credentials "/home/rpr/awsaccount.txt"))

(www-root-set! "/code/knozama/src/racket/main/knozama/static")

