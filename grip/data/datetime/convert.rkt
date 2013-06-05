#lang typed/racket/base

(provide:
 [local-tz-offset Integer]
 [current-date (case->
		(-> Datetime)
		(Integer -> Datetime))]
 [julian-day-number (Integer Integer Integer -> Integer)]
 [julian-day-number->date (Exact-Rational -> Date)]
 [decode-julian-day-number (Exact-Rational -> (Values Integer Date))]
 [date->julian-day-number (Date -> Integer)]
 [Datetime->InstantUTC (Datetime -> InstantUTC)])

(require
 racket/match
 (only-in "const.rkt"
	  sid sihd tai-epoch-in-jd)
 (only-in "instant.rkt"
	  now-utc instant-seconds instant-millis
	  instant-tai->utc)
 (only-in "leapsecond.rkt"
	  LeapMilliSecondTable
	  leap-millisecond-table)
 (only-in "types.rkt"
	  Tic
	  Instant-tics InstantUTC InstantTAI
	  JulianDay ModifiedJulianDay
	  Datetime Datetime-date Datetime-time
	  Date Date-year Date-month Date-day
	  Time Time-hour Time-minute Time-second Time-milli))

;; in Seconds, NOT millis
;; Uses built-in Racket calls here.
(: local-tz-offset Integer)
(define local-tz-offset
  (date-time-zone-offset (seconds->date (current-seconds))))

(: current-date (case->
		 (-> Datetime)
		 ( Integer -> Datetime)))
(define (current-date [offset local-tz-offset])
  (instant-utc->datetime (now-utc) offset))

;; gives the julian day which starts at noon.
(: julian-day-number (Integer Integer Integer -> Integer))
(define (julian-day-number day month year)
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

(: date->julian-day-number (Date -> Integer))
(define (date->julian-day-number date)
  (match date
	 ((Date y m d)
	  (julian-day-number d m y))))

(: julian-day-number->date (Exact-Rational -> Date))
(define (julian-day-number->date jd)
  (let-values (((t d) (decode-julian-day-number jd)))
    d))

(: dt->jd (Datetime -> Exact-Rational))
(define (dt->jd datetime)
  (match datetime
	 ((Datetime (Date year month day)
		    (Time offset hour minute second milli))
	  (+ (julian-day-number day month year)
	     (- 1/2)
	     (+ (/ (+ (* hour 60 60)
		      (* minute 60)
		      second
		      (/ milli 1000)
		      (- offset))
		   sid))))))

(: datetime->date (Datetime -> Date))
(define (datetime->date dt)
  (match dt
	 ((Datetime (Date year month day) _)
	  (Date year month day))))

(: date->datetime (Date -> Datetime))
(define (date->datetime date)
  (match date
	 ((Date year month day)
	  (Datetime date (Time local-tz-offset 0 0 0 0)))))

(: datetime->time (Datetime -> Time))
(define (datetime->time datetime)
  (match datetime
	 ((Datetime _ (Time offset hour minute second milli))
	  (Time offset hour minute second milli))))

;; ignores millis for now
(: seconds->julian-day-number (Integer Integer -> Exact-Rational))
(define (seconds->julian-day-number seconds tz-offset)
  (+ (/ (+ seconds
	   tz-offset
	   sihd)
	sid)
     tai-epoch-in-jd))

;; gives the seconds/date/month/year
(: decode-julian-day-number (Exact-Rational -> (Values Integer Date)))
(define (decode-julian-day-number jdn)
  (let* ((days (truncate jdn))
	 (a (+ days 32044))
	 (b (quotient (+ (* 4 a) 3) 146097))
	 (c (- a (quotient (* 146097 b) 4)))
	 (d (quotient (+ (* 4 c) 3) 1461))
	 (e (- c (quotient (* 1461 d) 4)))
	 (m (quotient (+ (* 5 e) 2) 153))
	 (y (+ (* 100 b) d -4800 (quotient m 10))))
    (values ; seconds (Date year month day)
     (assert (* (- jdn days) sid) exact-integer?)
     (Date (assert (if (>= 0 y) (- y 1) y) exact-integer?)
	   (assert (+ m 3 (* -12 (quotient m 10))) exact-integer?)
	   (assert (+ e (- (quotient (+ (* 153 m) 2) 5)) 1) exact-integer?)))))

(: datetime->julian-day (Datetime -> JulianDay))
(define (datetime->julian-day datetime)
  (JulianDay (dt->jd datetime)))

(: datetime->modified-julian-day (Datetime -> ModifiedJulianDay))
(define (datetime->modified-julian-day datetime)
  (ModifiedJulianDay (- (dt->jd datetime) 4800001/2)))

(: instant->datetime (InstantUTC (Option Integer) -> Datetime))
(define (instant->datetime instant offset)
  (let ((offset (if offset offset local-tz-offset)))
    (let-values (((secs date)
		  (decode-julian-day-number (seconds->julian-day-number (instant-seconds instant) offset))))
      (let* ((hours    (quotient secs (* 60 60)))
	     (rem      (remainder secs (* 60 60)))
	     (minutes  (quotient rem 60))
	     (seconds  (remainder rem 60))
	     (millis   (remainder (instant-millis instant) 1000)))
	(Datetime date (Time offset hours minutes seconds millis))))))

(: instant-tai->datetime (InstantTAI (Option Integer) -> Datetime))
(define (instant-tai->datetime instant tz-offset)

  (: find (((Pair Nonnegative-Integer Nonnegative-Integer) -> Boolean) LeapMilliSecondTable -> Boolean))
  (define (find proc l)
    (if (null? l)
	#f
	(if (proc (car l))
	    #t
	    (find proc (cdr l)))))

  (: tai-before-leap-second? (Integer -> Boolean))
  (define (tai-before-leap-second? second)
    (if (and (> second 0)
	     (exact-integer? second))
	(find (lambda: ((x : (Pair Nonnegative-Integer Nonnegative-Integer)))
		       (= second (- (+ (car x) (cdr x)) 1)))
	      leap-millisecond-table)
	#f))

  (: at-leap-sec-adjust (Datetime -> Datetime))
  (define (at-leap-sec-adjust dt)
    (match dt
	   ((Datetime date (Time offset hour minute second milli))
	    (Datetime date (Time offset hour minute 60 ;; at the leap second
				 milli)))))

  (let ((dt (instant->datetime (instant-tai->utc instant) tz-offset)))
    (if (tai-before-leap-second? (instant-seconds instant))
	(at-leap-sec-adjust dt)
	dt)))

(: instant-utc->datetime (InstantUTC (Option Integer) -> Datetime))
(define (instant-utc->datetime time tz-offset)
  (instant->datetime time tz-offset))

(: Datetime->InstantUTC (Datetime -> InstantUTC))
(define (Datetime->InstantUTC dt)
  (match dt
	 ((Datetime (Date year month day)
		    (Time offset hour minute second milli))
	  (let ((jdays (- (julian-day-number day month year)
			  tai-epoch-in-jd)))
	    (InstantUTC
	     (assert (+ (* (- jdays 1/2) 24 60 60)
			(* hour 60 60)
			(* minute 60)
			second
			milli
			(- offset)) exact-integer?))))))

;; (define (time-utc->julian-day time)
;;   (unless (eq? (time-type time) time-utc)
;;     (tm:time-error 'time->date 'incompatible-time-types  time))
;;   (+ (/ (+ (time-second time) (/ (time-nanosecond time) tm:nano))
;;         tm:sid)
;;      tm:tai-epoch-in-jd))

;; (define (time-utc->modified-julian-day time)
;;   (- (time-utc->julian-day time)
;;      4800001/2))

;; (define (time-tai->julian-day time)
;;   (unless (eq? (time-type time) time-tai)
;;     (tm:time-error 'time->date 'incompatible-time-types  time))
;;   (+ (/ (+ (- (time-second time)
;;               (tm:leap-second-delta (time-second time)))
;;            (/ (time-nanosecond time) tm:nano))
;;         tm:sid)
;;      tm:tai-epoch-in-jd))

;; (define (time-tai->modified-julian-day time)
;;   (- (time-tai->julian-day time)
;;      4800001/2))


;; (define (julian-day->time-utc jdn)
;;   (let ( (nanosecs (* tm:nano tm:sid (- jdn tm:tai-epoch-in-jd))) )
;;     (make-time time-utc
;;                (remainder nanosecs tm:nano)
;;                (floor (/ nanosecs tm:nano)))))

;; (define (julian-day->time-tai jdn)
;;   (time-utc->time-tai! (julian-day->time-utc jdn)))

;; (define (julian-day->time-monotonic jdn)
;;   (time-utc->time-monotonic! (julian-day->time-utc jdn)))

;; (define (julian-day->date jdn . tz-offset)
;;   (let ((offset (:optional tz-offset (tm:local-tz-offset))))
;;     (time-utc->date (julian-day->time-utc jdn) offset)))

;; (define (modified-julian-day->date jdn . tz-offset)
;;   (let ((offset (:optional tz-offset (tm:local-tz-offset))))
;;     (julian-day->date (+ jdn 4800001/2) offset)))

;; (define (modified-julian-day->time-utc jdn)
;;   (julian-day->time-utc (+ jdn 4800001/2)))

;; (define (modified-julian-day->time-tai jdn)
;;   (julian-day->time-tai (+ jdn 4800001/2)))

;; (define (modified-julian-day->time-monotonic jdn)
;;   (julian-day->time-monotonic (+ jdn 4800001/2)))

;; (define (current-julian-day)
;;   (time-utc->julian-day (current-time time-utc)))

;; (define (current-modified-julian-day)
;;   (time-utc->modified-julian-day (current-time time-utc)))
