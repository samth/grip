#lang typed/racket/base

(provide: 
 [starts-with-char? (String Char -> Boolean)]
 [ends-with-char? (String Char -> Boolean)]
 [starts-with? (String String -> Boolean)]
 [string-first-char-occurence (String Char -> (Option Index))])

(: starts-with-char? (String Char -> Boolean))
(define (starts-with-char? s ch)
  (if (zero? (string-length s))
      #f
      (char=? (string-ref s 0) ch)))

(: ends-with-char? (String Char -> Boolean))
(define (ends-with-char? s ch)
 (let ((len (string-length s)))
   (if (zero? len)           
       #f
       (char=? (string-ref s (sub1 len)) ch))))

(: starts-with? (String String -> Boolean))
(define (starts-with? s prefix)
  (let ((plen (string-length prefix)))
    (if (>= (string-length s)
            plen)
        (string=? (substring s 0 plen) prefix)
        #f)))

(: string-first-char-occurence (String Char -> (Option Index)))
(define (string-first-char-occurence s ch)
  (let ((len (string-length s)))
    (let loop ((i 0))
      (if (eq? i len)
          #f
          (if (char=? (string-ref s i) ch)
              (assert i index?)
              (loop (add1 i)))))))
              
