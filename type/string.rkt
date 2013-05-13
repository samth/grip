#lang typed/racket/base

(provide: 
 [null-string? (String -> Boolean)]
 [default-string (String String -> String)]
 [starts-with-char? (String Char -> Boolean)]
 [ends-with-char? (String Char -> Boolean)]
 [starts-with? (String String -> Boolean)]
 [string-first-char-occurence (String Char -> (Option Index))]
 [string-common-prefix-length (String String -> Index)]
 [string-relative-to-prefix  (String String -> String)])

(require 
 (only-in "opt.rkt"
          opt-apply-orelse))

(: null-string? (String -> Boolean))
(define (null-string? s)  
  (if (string? s)
      (zero? (string-length s))
      #f))

(define (default-string str default)
  (if (null-string? str)
      default
      str))

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

(: string-common-prefix-length (String String -> Index))
(define (string-common-prefix-length prefix s)
  (let: ((len (min (string-length s)
                   (string-length prefix))))
    (let loop ((i 0))
      (if (and (< i len)
               (char=? (string-ref prefix i)
                       (string-ref s      i)))
          (loop (add1 i))
          (assert i index?)))))

(: string-relative-to-prefix (String String -> String))
(define (string-relative-to-prefix prefix s)                     
  (substring s (string-common-prefix-length prefix s)))

;  (opt-apply-orelse (string-common-prefix-index prefix s) (λ: ((i : Index)) (substring s i)) s))

