#lang typed/racket/base

(provide
 string->real
 string->integer)

(: string->real (String -> (Option Real)))
(define (string->real str)
  (let ((real (string->number str)))
    (if (real? real)
       real
       #f)))

(: string->integer (String -> (Option Integer)))
(define (string->integer str)
  (let ((int (string->number str)))
    (if (exact-integer? int)
       int
       #f)))
