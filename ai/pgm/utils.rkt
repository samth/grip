#lang typed/racket/base

(provide 
 sort-variables)

(: sort-variables ((Listof Symbol) -> (Listof Symbol)))
(define (sort-variables vars)  
  (: var<? (Symbol Symbol -> Boolean))
  (define (var<? k1 k2)
    (let ((s1 (symbol->string k1))
	  (s2 (symbol->string k2)))
      (string<=? s1 s2)))  
  (sort vars var<?))


