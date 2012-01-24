#lang typed/racket/base

(provide 
 sts-host
 get-session-token-action)

(define sts-host "sts.amazon.aws.com")

(define get-session-token-action "GetSessionToken")
