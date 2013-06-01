#lang typed/racket/base

(require 
 (only-in "const.rkt"
	  sid sihd tai-epoch-in-jd)
 (only-in "util.rkt"
	  encode-julian-day-number)
 (only-in "datetime.rkt"
	  Datetime Datetime? Datetime-offset Datetime-year Datetime-month 
	  Datetime-day Datetime-hour Datetime-minute Datetime-second Datetime-milli))

(: datetime->julian-day (Datetime -> Exact-Rational))
(define (datetime->julian-day datetime)
  (let ((milli (Datetime-milli datetime))
	(second (Datetime-second datetime))
	(minute (Datetime-minute datetime))
	(hour (Datetime-hour datetime))
	(day (Datetime-day datetime))
	(month (Datetime-month datetime))
	(year (Datetime-year datetime))
	(offset (Datetime-offset datetime)))
    (+ (encode-julian-day-number day month year)
       (- 1/2)
       (+ (/ (+ (* hour 60 60)
                (* minute 60)
                second
                (/ milli 1000)
                (- offset))
             sid)))))

(: datetime->modified-julian-day (Datetime -> Exact-Rational))
(define (datetime->modified-julian-day datetime)
  (- (datetime->julian-day datetime)
     4800001/2))

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

