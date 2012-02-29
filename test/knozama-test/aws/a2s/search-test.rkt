#lang racket/base

(require 
 knozama/aws/credentials
 knozama/aws/configuration
 knozama/aws/a2s/search)

(define creds (load-credential aws-credential-path))

