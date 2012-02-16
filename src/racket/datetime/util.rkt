#lang typed/racket/base

(provide 
 time->julian-day-number
 decode-julian-day-number
 encode-julian-day-number)

(require
 (only-in "const.rkt"
	  sid sihd tai-epoch-in-jd))

;; ignores millis
(: time->julian-day-number (Integer Integer -> Exact-Rational))
(define (time->julian-day-number seconds tz-offset)
  (+ (/ (+ seconds
	   tz-offset
	   sihd)
	sid)
     tai-epoch-in-jd))

;; gives the seconds/date/month/year
(: decode-julian-day-number (Exact-Rational -> (Values Integer Integer Integer Integer)))
(define (decode-julian-day-number jdn)
  (let* ((days (truncate jdn))
         (a (+ days 32044))
         (b (quotient (+ (* 4 a) 3) 146097))
         (c (- a (quotient (* 146097 b) 4)))
         (d (quotient (+ (* 4 c) 3) 1461))
         (e (- c (quotient (* 1461 d) 4)))
         (m (quotient (+ (* 5 e) 2) 153))
         (y (+ (* 100 b) d -4800 (quotient m 10))))
    (values ; seconds date month year
     (assert (* (- jdn days) sid) exact-integer?)
     (assert (+ e (- (quotient (+ (* 153 m) 2) 5)) 1) exact-integer?)
     (assert (+ m 3 (* -12 (quotient m 10))) exact-integer?)
     (assert (if (>= 0 y) (- y 1) y) exact-integer?))))

;; gives the julian day which starts at noon.
(: encode-julian-day-number (Integer Integer Integer -> Integer))
(define (encode-julian-day-number day month year)
  (let* ((a (quotient (- 14 month) 12))
         (y (- (+ year 4800) a (if (negative? year) -1  0)))
         (m (- (+ month (* 12 a)) 3)))
    (+ day
       (quotient (+ (* 153 m) 2) 5)
       (* 365 y)
       (quotient y 4)
       (- (quotient y 100))
       (quotient y 400)
       -32045)))
