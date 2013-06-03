#lang typed/racket/base

(provide
 sid sihd tai-epoch-in-jd days-in-week)

(define sid  86400)		; seconds in a day
(define sihd 43200)		; seconds in a half day
(define tai-epoch-in-jd 4881175/2) ; julian day number for 'the epoch'
(define days-in-week 7)
