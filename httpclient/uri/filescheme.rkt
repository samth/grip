#| Routines applicable to file: schema URIs |#

#lang typed/racket/base

(provide:
 [local-file-uri? (Uri -> Boolean)]
 [local-file-uri->path (Uri -> Path)])

(require 
 (only-in "../uri.rkt"
          Uri Uri-scheme Uri-path Uri-authority Authority-host))

(: local-file-uri? (Uri -> Boolean))
(define (local-file-uri? uri)
  (and (string=? (Uri-scheme uri) "file")
       (eq? #f (Uri-authority uri))))      

(: local-file-uri->path (Uri -> Path))
(define (local-file-uri->path uri)
  (if (local-file-uri? uri)
      (string->path (Uri-path uri))
      (error 'file-uri->local-path "Uri has a host or is not a file: ~s" uri)))
