#lang typed/racket/base

(provide
 Sxml SXPath
 html->sxml)

(require/typed 
 (planet neil/htmlprag:1:6)
 (html->sxml (Input-Port -> (Listof Any))))

(define-type Sxml (Listof Any))

(define-type SXPath (Sxml -> Sxml))
