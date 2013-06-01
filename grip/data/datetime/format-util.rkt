#lang typed/racket/base

(provide 
 padding
 localized-message
 last-n-digits)

;; returns a string rep. of number N, of minimum LENGTH,
;; padded with character PAD-WITH. If PAD-WITH if #f, 
;; no padding is done, and it's as if number->string was used.
;; if string is longer than LENGTH, it's as if number->string was used.
(: padding (Integer (Option Char) Integer -> String))
(define (padding n pad-with length)
  (let* ( (str (number->string n))
          (str-len (string-length str)) )
    (if (or (> str-len length)
            (not pad-with))
        str
        (let* ( (new-str (make-string length pad-with))
                (new-str-offset (- (string-length new-str)
                                   str-len)) )
          (do ((i 0 (+ i 1)))
            ((>= i (string-length str)))
            (string-set! new-str (+ new-str-offset i) 
                         (string-ref str i)))
          new-str))))

(: last-n-digits (Integer Integer -> Integer))
(define (last-n-digits i n)
  (abs (remainder i (assert (expt 10 n) integer?))))

(: localized-message (Symbol -> String))
(define (localized-message x) (symbol->string x))
