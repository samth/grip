#| Routines applicable to file: schema URIs |#

#lang typed/racket/base

(provide:
 [local-path->uri (Path -> Uri)]
 [local-file-uri? (Uri -> Boolean)]
 [local-file-uri->path (Uri -> Path)])

(require 
 (only-in "../uri.rkt"
          parse-uri
          Uri Uri-scheme Uri-path Uri-authority Authority-host))

(: local-file-uri? (Uri -> Boolean))
(define (local-file-uri? uri)
  (and (string=? (Uri-scheme uri) "file")
       (eq? #f (Uri-authority uri))))

(: local-file-uri->path (Uri -> Path))
(define (local-file-uri->path uri)  
  (string->path (Uri-path uri)))

; no host name
(: local-path->uri (Path -> Uri))
(define (local-path->uri path)
  (assert (parse-uri (string-append "file://" (path->string path)))))

