#lang typed/racket/base

(provide 
 parse-tab-line)

(require/typed racket/string
  [string-trim (String -> String)]
  [string-split (String (U String Regexp) -> (Listof String))])

(: parse-tab-line (String -> (Listof String)))
(define (parse-tab-line str)
  (map string-trim (string-split str "\t")))
  
