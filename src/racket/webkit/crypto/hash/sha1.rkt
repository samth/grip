#lang typed/racket/base

(provide sha1-bytes)

(require/typed openssl/sha1
	       ((sha1-bytes sha1-inp) (Input-Port -> Bytes)))

(: sha1-bytes  (Bytes -> Bytes))
(define (sha1-bytes bytes)
  (sha1-inp (open-input-bytes bytes)))


