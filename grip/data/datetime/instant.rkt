#lang typed/racket/base

(provide:
 [now-tai (-> InstantTAI)]
 [now-utc (-> InstantUTC)]
 [instant=?   (Instant Instant -> Boolean)]
 [instant>?   (Instant Instant -> Boolean)]
 [instant<?   (Instant Instant -> Boolean)]
 [instant>=?  (Instant Instant -> Boolean)]
 [instant<=?  (Instant Instant -> Boolean)]
 [interval    (Instant Instant -> Interval)]
 [future-past (All (SomeInst) Instant Interval (InstantMaker SomeInst) -> SomeInst)]

 [instant-tai->utc (InstantTAI -> InstantUTC)]
 [instant-utc->tai (InstantUTC -> InstantTAI)]
 [instant-millis   (Instant -> Integer)]
 [instant-seconds  (Instant -> Integer)]
 [instant-seconds-millis-remainder (Instant -> (Values Integer Integer))])

(require
 (only-in "types.rkt"
	  Tic Interval Interval-tics
	  Instant InstantTAI InstantTAI? 
	  InstantUTC InstantUTC?
	  Instant-tics InstantMaker)
 (only-in "leapsecond.rkt"
	  leap-second-delta
	  leap-second-neg-delta))

(: instant-millis (Instant -> Integer))
(define instant-millis Instant-tics)

(: instant-seconds (Instant -> Integer))
(define (instant-seconds instant)
  (inexact->exact (floor (/ (instant-millis instant) 1000))))

(: instant-seconds-millis-remainder (Instant -> (Values Integer Integer)))
(define (instant-seconds-millis-remainder instant)
  (let ((secs (instant-seconds instant)))
    (values secs (- (instant-millis instant) (* secs 1000)))))

;; Now in millisecs TAI
(: now-tai (-> InstantTAI))
(define (now-tai)
  (instant-utc->tai (now-utc)))

(: now-utc (-> InstantUTC))
(define (now-utc)
  (InstantUTC (assert (inexact->exact (round (current-inexact-milliseconds))) 
		      exact-integer?)))

(: base-tai-value (Instant -> Integer))
(define (base-tai-value inst)
  (let ((millis (Instant-tics inst)))
    (if (InstantTAI? inst)
	millis
	(utc->tai-value millis))))

(: instant-bin-op (All (A) ((Integer Integer -> A) Instant Instant -> A)))
(define (instant-bin-op op i1 i2)
  (let ((i1-millisl (base-tai-value i1))
	(i2-millis  (base-tai-value i2)))
    (op i1-millisl i2-millis)))

(: instant=? (Instant Instant -> Boolean))
(define (instant=? i1 i2)
  (instant-bin-op eqv? i1 i2))

(: instant>? (Instant Instant -> Boolean))
(define (instant>? inst1 inst2)
  (instant-bin-op > inst1 inst2))

(: instant<? (Instant Instant -> Boolean))
(define (instant<? inst1 inst2)
  (instant-bin-op < inst1 inst2))

(: instant>=? (Instant Instant -> Boolean))
(define (instant>=? inst1 inst2)
  (instant-bin-op >= inst1 inst2))

(: instant<=? (Instant Instant -> Boolean))
(define (instant<=? inst1 inst2)
  (instant-bin-op <= inst1 inst2))

(: interval (Instant Instant -> Interval))
(define (interval inst1 inst2)
  (Interval (instant-bin-op - inst1 inst2)))

(: future-past (All (SomeInst) Instant Interval (InstantMaker SomeInst) -> SomeInst))
(define (future-past inst inter maker)
  (maker (+ (Instant-tics inst) (Interval-tics inter))))

;; -- Converters between types.

(: instant-tai->utc (InstantTAI -> InstantUTC))
(define (instant-tai->utc time-tai)
  (let* ((tai (Instant-tics time-tai))
	 (adjustment (if (> tai 0)
			 (leap-second-neg-delta tai)
			 0)))
    (InstantUTC (- tai adjustment))))

(: utc->tai-value (Integer -> Integer))
(define (utc->tai-value utc)
  (+ utc (if (> utc 0)
	     (leap-second-delta utc)
	     0)))

(: instant-utc->tai (InstantUTC -> InstantTAI))
(define (instant-utc->tai time-utc)
  (InstantTAI (utc->tai-value (Instant-tics time-utc))))


