#lang typed/racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Knozama's Amazon API Library
;; Copyright (C) 2007,2008,2009,2010,2011  Raymond Paul Racine
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(struct: DateTime ([offset : Fixnum]
		   [year : Fixnum]
		   [month : Fixnum]
		   [day : Fixnum]
		   [hour : Fixnum]
		   [minute : Fixnum]
		   [second : Fixnum]
		   [millis : Fixnum]) #:transparent)

;; The means of measuring time or timekeeping
;;(struct: Chronometry () #:transparent)

;; Instant in time in millis along with a Chronometry
;; Assumes a 64 bit build of Racket for the long haul.
(struct: Instant ([millis : Integer]) #:transparent)
(struct: InstantTAI Instant () #:transparent)
(struct: InstantUTC Instant () #:transparent)

(struct: Duration Instant () #:transparent)

(define locale-abbr-weekday-vector (vector 'sun 'mon 'tue 'wed 'thu 'fri 'sat))
(define locale-long-weekday-vector (vector 'sunday 'monday 'tuesday 'wednesday
					   'thursday 'friday 'saturday))
;; note empty string in 0th place. 
(define locale-abbr-month-vector   (vector 'jan 'feb 'mar
					   'apr 'may 'jun 'jul
					   'aug 'sep 'oct 'nov
					   'dec))

(define locale-long-month-vector   (vector 'january 'february
					   'march 'april 'may
					   'june 'july 'august
					   'september 'october
					   'november 'december)) 

(define locale-pm 'pm)
(define locale-am 'am)

;; ;; See date->string
;; (define tm:locale-date-time-format 'date-time)
;; (define tm:locale-short-date-format 'date)
;; (define tm:locale-time-format 'time)
;; (define tm:iso-8601-date-time-format 'iso8601)
;; ;;-- Miscellaneous Constants.
;; ;;-- only the tm:tai-epoch-in-jd might need changing if
;; ;;   a different epoch is used.

;;(define tm:nano (expt 10 9))
(define sid  86400)		; seconds in a day
(define sihd 43200)		; seconds in a half day
(define tai-epoch-in-jd 4881175/2) ; julian day number for 'the epoch'

;; ;; A Very simple Error system for the time procedures
;; ;; 
;; (define tm:time-error-types
;;   '(invalid-clock-type
;;     unsupported-clock-type
;;     incompatible-time-types
;;     not-duration
;;     dates-are-immutable
;;     bad-date-format-string
;;     bad-date-template-string
;;     invalid-month-specification
;;     ))

;; (define (tm:time-error caller type value)
;;   (if (member type tm:time-error-types)
;;       (if value
;;           (error caller "TIME-ERROR type ~S: ~S" type value)
;;           (error caller "TIME-ERROR type ~S" type))
;;       (error caller "TIME-ERROR unsupported error type ~S" type)))

;; ;; A table of leap seconds
;; ;; See ftp://maia.usno.navy.mil/ser7/tai-utc.dat
;; ;; and update as necessary.
;; ;; this procedures reads the file in the above
;; ;; format and creates the leap second table
;; ;; it also calls the almost standard, but not R5 procedures read-line 
;; ;; & open-input-string
;; ;; ie (set! tm:leap-second-table (tm:read-tai-utc-date "tai-utc.dat"))

;; (define (tm:read-tai-utc-data filename)
;;   (define (convert-jd jd)
;;     (* (- (inexact->exact jd) tm:tai-epoch-in-jd) tm:sid))
;;   (define (convert-sec sec)
;;     (inexact->exact sec))
;;   (let ( (port (open-input-file filename))
;;          (table '()) )
;;     (let loop ((line (read-line port)))
;;       (unless (eq? line eof)
;;         (let* ( (data (read (open-input-string (string-append "(" line ")")))) 
;;                 (year (car data))
;;                 (jd   (cadddr (cdr data)))
;;                 (secs (cadddr (cdddr data))) )
;;           (when (>= year 1972)
;;             (set! table (cons (cons (convert-jd jd) (convert-sec secs)) table)))
;;           (loop (read-line port)))))
;;     table))


;; See Wikipedia entry for leap seconds.
;; FIX ME RPR We are missing some leap seconds here.
(define-type LeapSecondTable (Listof (Pair Exact-Positive-Integer Positive-Byte)))

(: leap-second-table LeapSecondTable)
(define leap-second-table
  '((1136073600000 . 33)
    (915148800000  . 32)
    (867715200000  . 31)
    (820454400000  . 30)
    (773020800000  . 29)
    (741484800000  . 28)
    (709948800000  . 27)
    (662688000000  . 26)
    (631152000000  . 25)
    (567993600000  . 24)
    (489024000000  . 23)
    (425865600000  . 22)
    (394329600000  . 21)
    (362793600000  . 20)
    (315532800000  . 19)
    (283996800000  . 18)
    (252460800000  . 17)
    (220924800000  . 16)
    (189302400000  . 15)
    (157766400000  . 14)
    (126230400000  . 13)
    (94694400000   . 12)
    (78796800000   . 11)
    (63072000000   . 10)))

(: no-leap-instant Integer)
(define no-leap-instant (* (- 1972 1970) 365 sid))

;; Given milliseconds UTC.
;; Number of leap seconds between TAI and UTC, i.e. TAI - UTC
(: leap-second-delta (Integer -> Byte))
(define (leap-second-delta utc-milli-seconds)
  (let ((utc-seconds (/ utc-milli-seconds 1000)))
    (if (< utc-seconds no-leap-instant)
	0
	(do: : Positive-Byte ([table : LeapSecondTable leap-second-table (cdr table)])
	     ((>= utc-seconds (caar table)) (cdar table))))))

;; going from tai seconds to utc seconds ... 
(: leap-second-neg-delta (Exact-Positive-Integer -> Byte))
(define (leap-second-neg-delta tai-seconds)
  (if (< tai-seconds no-leap-instant)
      0
      (let: loop : Byte ([table : LeapSecondTable leap-second-table])
	    (cond
	     ((null? table) 0)
	     ((<= (cdar table) 
		  (- tai-seconds (caar table)))
	      (cdar (assert table pair?)))
	     (else (loop (cdr table)))))))

;; (define (time-type t)       (tm:time-ref t 0))
;; (define (time-nanosecond t) (tm:time-ref t 1))
;; (define (time-second t)     (tm:time-ref t 2))

;; (define (set-time-type! t type)     (tm:time-set! t 0 type))
;; (define (set-time-nanosecond! t ns) (tm:time-set! t 1 ns))
;; (define (set-time-second! t s)      (tm:time-set! t 2 s))

;; (define (copy-time time)
;;   (let ((ntime (make-time #f #f #f)))
;;     (set-time-type! ntime (time-type time))
;;     (set-time-second! ntime (time-second time))
;;     (set-time-nanosecond! ntime (time-nanosecond time))
;;     ntime))


;; Now in millisecs UTC
(: instant-now (-> Instant))
(define (instant-now)
  (Instant (assert (inexact->exact (floor (current-inexact-milliseconds))) exact-integer?)))

;; (define (tm:current-time-tai)
;;   (receive (seconds ms) (tm:get-time-of-day)
;;            (make-time time-tai
;;                       (* ms 1000000)
;;                       (+ seconds (tm:leap-second-delta seconds))
;;                       )))

(: instant=? (Instant Instant -> Boolean))
(define (instant=? time1 time2)
  (eqv? (Instant-millis time1) (Instant-millis time2)))

(: instant>? (Instant Instant -> Boolean))
(define (instant>? inst1 inst2)
  (> (Instant-millis inst1) (Instant-millis inst2)))

(: instant<? (Instant Instant -> Boolean))
(define (instant<? inst1 inst2)
  (< (Instant-millis inst1) (Instant-millis inst2)))

(: instant>=? (Instant Instant -> Boolean))
(define (instant>=? inst1 inst2)
  (>= (Instant-millis inst1) (Instant-millis inst2)))

(: instant<=? (Instant Instant -> Boolean))
(define (instant<=? inst1 inst2)
  (<= (Instant-millis inst1) (Instant-millis inst2)))

(: interval (Instant Instant -> Duration))
(define (interval inst1 inst2)
  (Duration (- (Instant-millis inst1) (Instant-millis inst2))))

(: add-interval (Instant Duration -> Instant))
(define (add-interval inst inter)
  (Instant (+ (Instant-millis inst) (Instant-millis inter))))

(: subtract-interval (Instant Instant -> Instant))
(define (subtract-interval inst inter)
  (Instant (- (Instant-millis inst) (Instant-millis inter))))

;; -- Converters between types.

(: instant-tai->utc (InstantTAI -> InstantUTC))
(define (instant-tai->utc time-tai)
  (let* ((tai (Instant-millis time-tai))
	 (adjustment (if (> tai 0)
			 (leap-second-neg-delta tai)
			 0)))
  (InstantUTC (- tai adjustment))))

(: instant-utc->tai (InstantUTC -> InstantTAI))
(define (instant-utc->tai time-utc)
  (let* ((utc (Instant-millis time-utc))
	 (adj (if (> utc 0)
		  (leap-second-delta utc)
		  0)))
    (InstantTAI (+ utc adj))))

;; gives the julian day which starts at noon.
(: encode-julian-day-number (Fixnum Fixnum Fixnum -> Integer))
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

;; (define (tm:char-pos char str index len)
;;   (cond
;;     ((>= index len) #f)
;;     ((char=? (string-ref str index) char)
;;      index)
;;     (else
;;      (tm:char-pos char str (+ index 1) len))))

;; ; return a string representing the decimal expansion of the fractional 
;; ; portion of a number, limited by a specified precision
;; (define (tm:decimal-expansion r precision)
;;   (let loop ([num (- r (round r))]
;;              [p precision])
;;     (if (or (= p 0) (= num 0))
;;         ""
;;         (let* ([num-times-10 (* 10 num)]
;;                [round-num-times-10 (round num-times-10)]) 
;;           (string-append (number->string (inexact->exact round-num-times-10))
;;                          (loop (- num-times-10 round-num-times-10) (- p 1)))))))

;; gives the seconds/date/month/year
(: decode-julian-day-number (Integer -> (Values Integer Integer Integer Integer)))
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
     (* (- jdn days) sid)
     (+ e (- (quotient (+ (* 153 m) 2) 5)) 1)
     (+ m 3 (* -12 (quotient m 10)))
     (if (>= 0 y) (- y 1) y))))

(define local-tz-offset
  (date-time-zone-offset (seconds->date (current-seconds))))

;; ignores millis
(: milliseconds->julian-day-number (Integer Integer -> Exact-Rational))
(define (milliseconds->julian-day-number millisecs tz-offset)
  (let ((seconds (floor (/ millisecs 1000))))
    (+ (/ (+ seconds
	     tz-offset
	     sihd)
	  sid)
       tai-epoch-in-jd)))

;; (define (tm:find proc l)
;;   (if (null? l)
;;       #f
;;       (if (proc (car l))
;;           #t
;;           (tm:find proc (cdr l)))))

;; (define (tm:tai-before-leap-second? second)
;;   (tm:find (lambda (x)
;;              (= second (- (+ (car x) (cdr x)) 1)))
;;            tm:leap-second-table))

;; (define (tm:time->date time tz-offset ttype)
;;   (unless (eq? (time-type time) ttype)
;;     (tm:time-error 'time->date 'incompatible-time-types  time))
;;   (let* ( (offset (:optional tz-offset (tm:local-tz-offset))) )
;;     (receive (secs date month year)
;;              (tm:decode-julian-day-number
;;               (tm:time->julian-day-number (time-second time) offset))
;;              (let* ( (hours    (quotient secs (* 60 60)))
;;                      (rem      (remainder secs (* 60 60)))
;;                      (minutes  (quotient rem 60))
;;                      (seconds  (remainder rem 60)) )
;;                (srfi:make-date (time-nanosecond time)
;;                                seconds
;;                                minutes
;;                                hours
;;                                date
;;                                month
;;                                year
;;                                offset)))))


;; START HERE START HERE START HERE

;; (: instant->date-time (Instant (Option Integer) -> DateTime))
;; (define (instant->date-time time offset ttype)
;;   (let* ((offset (if offset offset local-tz-offset))
;; 	 (receive (secs date month year)
;;              (decode-julian-day-number
;;               (tm:time->julian-day-number (time-second time) offset))
;;              (let* ( (hours    (quotient secs (* 60 60)))
;;                      (rem      (remainder secs (* 60 60)))
;;                      (minutes  (quotient rem 60))
;;                      (seconds  (remainder rem 60)) )
;;                (srfi:make-date (time-nanosecond time)
;;                                seconds
;;                                minutes
;;                                hours
;;                                date
;;                                month
;;                                year
;;                                offset)))))




;; (define (time-tai->date time . tz-offset)
;;   (if (tm:tai-before-leap-second? (time-second time))
;;       ;; if it's *right* before the leap, we need to pretend to subtract a second ...

;;         (tm:set-date-second! d 60)
;;         d)
;;       (tm:time->date (time-tai->time-utc time) tz-offset time-utc)))

;; (define (time-utc->date time . tz-offset)
;;   (tm:time->date time tz-offset time-utc))

;; ;; again, time-monotonic is the same as time tai
;; (define (time-monotonic->date time . tz-offset)
;;   (tm:time->date time tz-offset time-monotonic))

;; (define (date->time-utc date)
;;   (let ( (nanosecond (date-nanosecond date))
;;          (second (srfi:date-second date))
;;          (minute (srfi:date-minute date))
;;          (hour (srfi:date-hour date))
;;          (day (srfi:date-day date))
;;          (month (srfi:date-month date))
;;          (year (srfi:date-year date))
;;          (offset (date-zone-offset date)) )
;;     (let ( (jdays (- (tm:encode-julian-day-number day month year)
;;                      tm:tai-epoch-in-jd)) )
;;       (make-time 
;;        time-utc
;;        nanosecond
;;        (+ (* (- jdays 1/2) 24 60 60)
;;           (* hour 60 60)
;;           (* minute 60)
;;           second
;;           (- offset))
;;        ))))

;; (define (date->time-tai d)
;;   (if (= (srfi:date-second d) 60)
;;       (subtract-duration! (time-utc->time-tai! (date->time-utc d)) (make-time time-duration 0 1))
;;       (time-utc->time-tai! (date->time-utc d))))

;; (define (date->time-monotonic date)
;;   (time-utc->time-monotonic! (date->time-utc date)))


;; (define (tm:leap-year? year)
;;   (or (= (modulo year 400) 0)
;;       (and (= (modulo year 4) 0) (not (= (modulo year 100) 0)))))

;; (define (leap-year? date)
;;   (tm:leap-year? (srfi:date-year date)))

;; ;; tm:year-day fixed: adding wrong number of days.
;; (define  tm:month-assoc '((0 . 0) (1 . 31)  (2 . 59)   (3 . 90)   (4 . 120) 
;;                                   (5 . 151) (6 . 181)  (7 . 212)  (8 . 243)
;;                                   (9 . 273) (10 . 304) (11 . 334)))

;; (define (tm:year-day day month year)
;;   (let ((days-pr (assoc (- month 1) tm:month-assoc)))
;;     (unless days-pr
;;       (tm:time-error 'date-year-day 'invalid-month-specification month))
;;     (if (and (tm:leap-year? year) (> month 2))
;;         (+ day (cdr days-pr) 1)
;;         (+ day (cdr days-pr)))))

;; (define (srfi:date-year-day date)
;;   (tm:year-day (srfi:date-day date) (srfi:date-month date) (srfi:date-year date)))

;; ;; from calendar faq 
;; (define (tm:week-day day month year)
;;   (let* ((a (quotient (- 14 month) 12))
;;          (y (- year a))
;;          (m (+ month (* 12 a) -2)))
;;     (modulo (+ day y (quotient y 4) (- (quotient y 100))
;;                (quotient y 400) (quotient (* 31 m) 12))
;;             7)))

;; (define (srfi:date-week-day date)
;;   (tm:week-day (srfi:date-day date) (srfi:date-month date) (srfi:date-year date)))

;; (define (tm:days-before-first-week date day-of-week-starting-week)
;;   (let* ( (first-day (srfi:make-date 0 0 0 0
;;                                      1
;;                                      1
;;                                      (srfi:date-year date)
;;                                      #f))
;;           (fdweek-day (srfi:date-week-day first-day))  )
;;     (modulo (- day-of-week-starting-week fdweek-day)
;;             7)))

;; (define (date-week-number date day-of-week-starting-week)
;;   (quotient (- (srfi:date-year-day date)
;;                (tm:days-before-first-week  date day-of-week-starting-week))
;;             7))

;; (define (current-date . tz-offset) 
;;   (time-utc->date (current-time time-utc)
;;                   (:optional tz-offset (tm:local-tz-offset))))

;; ;; given a 'two digit' number, find the year within 50 years +/-
;; (define (tm:natural-year n)
;;   (let* ( (current-year (srfi:date-year (current-date)))
;;           (current-century (* (quotient current-year 100) 100)) )
;;     (cond
;;       ((>= n 100) n)
;;       ((<  n 0) n)
;;       ((<=  (- (+ current-century n) current-year) 50)
;;        (+ current-century n))
;;       (else
;;        (+ (- current-century 100) n)))))

;; (define (date->julian-day date)
;;   (let ( (nanosecond (date-nanosecond date))
;;          (second (srfi:date-second date))
;;          (minute (srfi:date-minute date))
;;          (hour (srfi:date-hour date))
;;          (day (srfi:date-day date))
;;          (month (srfi:date-month date))
;;          (year (srfi:date-year date))
;;          (offset (date-zone-offset date)) )
;;     (+ (tm:encode-julian-day-number day month year)
;;        (- 1/2)
;;        (+ (/ (+ (* hour 60 60)
;;                 (* minute 60)
;;                 second
;;                 (/ nanosecond tm:nano)
;;                 (- offset))
;;              tm:sid)))))

;; (define (date->modified-julian-day date)
;;   (- (date->julian-day date)
;;      4800001/2))


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

;; ;; this is the same as time-tai->julian-day
;; (define (time-monotonic->julian-day time)
;;   (unless (eq? (time-type time) time-monotonic)
;;     (tm:time-error 'time->date 'incompatible-time-types  time))
;;   (+ (/ (+ (- (time-second time) 
;;               (tm:leap-second-delta (time-second time)))
;;            (/ (time-nanosecond time) tm:nano))
;;         tm:sid)
;;      tm:tai-epoch-in-jd))


;; (define (time-monotonic->modified-julian-day time)
;;   (- (time-monotonic->julian-day time)
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

;; ;; returns a string rep. of number N, of minimum LENGTH,
;; ;; padded with character PAD-WITH. If PAD-WITH if #f, 
;; ;; no padding is done, and it's as if number->string was used.
;; ;; if string is longer than LENGTH, it's as if number->string was used.
;; (define (tm:padding n pad-with length)
;;   (let* ( (str (number->string n))
;;           (str-len (string-length str)) )
;;     (if (or (> str-len length)
;;             (not pad-with))
;;         str
;;         (let* ( (new-str (make-string length pad-with))
;;                 (new-str-offset (- (string-length new-str)
;;                                    str-len)) )
;;           (do ((i 0 (+ i 1)))
;;             ((>= i (string-length str)))
;;             (string-set! new-str (+ new-str-offset i) 
;;                          (string-ref str i)))
;;           new-str))))

;; (define (tm:last-n-digits i n)
;;   (abs (remainder i (expt 10 n))))

;; (define (tm:locale-abbr-weekday n) 
;;   (localized-message (vector-ref tm:locale-abbr-weekday-vector n)))

;; (define (tm:locale-long-weekday n)
;;   (localized-message (vector-ref tm:locale-long-weekday-vector n)))

;; (define (tm:locale-abbr-month n)
;;   (localized-message (vector-ref tm:locale-abbr-month-vector (- n 1))))

;; (define (tm:locale-long-month n)
;;   (localized-message (vector-ref tm:locale-long-month-vector (- n 1))))

;; (define (tm:vector-find needle haystack comparator)
;;   (let ((len (vector-length haystack)))
;;     (define (tm:vector-find-int index)
;;       (cond
;;         ((>= index len) #f)
;;         ((comparator needle (localized-message (vector-ref haystack index))) (+ index 1))
;;         (else (tm:vector-find-int (+ index 1)))))
;;     (tm:vector-find-int 0)))

;; (define (tm:locale-abbr-weekday->index string)
;;   (tm:vector-find string tm:locale-abbr-weekday-vector string=?))

;; (define (tm:locale-long-weekday->index string)
;;   (tm:vector-find string tm:locale-long-weekday-vector string=?))

;; (define (tm:locale-abbr-month->index string)
;;   (tm:vector-find string tm:locale-abbr-month-vector string=?))

;; (define (tm:locale-long-month->index string)
;;   (tm:vector-find string tm:locale-long-month-vector string=?))



;; ;; do nothing. 
;; ;; Your implementation might want to do something...
;; ;; 
;; (define (tm:locale-print-time-zone date port)
;;   (values))

;; ;; Again, locale specific.
;; (define (tm:locale-am/pm hr)
;;   (localized-message
;;    (if (> hr 11) tm:locale-pm tm:locale-am)))

;; (define (tm:tz-printer offset port)
;;   (display
;;    (cond [(= offset 0) "Z"]
;;          [else (let ([sign (cond [(negative? offset) "-"]
;;                                  [else               "+"])]
;;                      [hours (abs (quotient offset (* 60 60)))]
;;                      [minutes (abs (quotient (remainder offset (* 60 60)) 60))])
;;                  (string-append sign (tm:padding hours #\0 2) (tm:padding minutes #\0 2)))])
;;    port))

;; ;; A table of output formatting directives.
;; ;; the first time is the format char.
;; ;; the second is a procedure that takes the date, a padding character
;; ;; (which might be #f), and the output port.
;; ;;
;; (define tm:directives 
;;   (list
;;    (cons #\~ (lambda (date pad-with port) (display #\~ port)))
   
;;    (cons #\a (lambda (date pad-with port)
;;                (display (tm:locale-abbr-weekday (srfi:date-week-day date))
;;                         port)))
;;    (cons #\A (lambda (date pad-with port)
;;                (display (tm:locale-long-weekday (srfi:date-week-day date))
;;                         port)))
;;    (cons #\b (lambda (date pad-with port)
;;                (display (tm:locale-abbr-month (srfi:date-month date))
;;                         port)))
;;    (cons #\B (lambda (date pad-with port)
;;                (display (tm:locale-long-month (srfi:date-month date))
;;                         port)))
;;    (cons #\c (lambda (date pad-with port)
;;                (display (date->string date (localized-message tm:locale-date-time-format)) port)))
;;    (cons #\d (lambda (date pad-with port)
;;                (display (tm:padding (srfi:date-day date)
;;                                     #\0 2)
;;                         port)))
;;    (cons #\D (lambda (date pad-with port)
;;                (display (date->string date "~m/~d/~y") port)))
;;    (cons #\e (lambda (date pad-with port)
;;                (display (tm:padding (srfi:date-day date)
;;                                     #\Space 2)
;;                         port)))
;;    (cons #\f (lambda (date pad-with port)
;;                (if (> (date-nanosecond date)
;;                       tm:nano)
;;                    (display (tm:padding (+ (srfi:date-second date) 1)
;;                                         pad-with 2)
;;                             port)
;;                    (display (tm:padding (srfi:date-second date)
;;                                         pad-with 2)
;;                             port))
;;                (let ([f (tm:decimal-expansion (/ (date-nanosecond date) tm:nano) 6)])
;;                  (when (> (string-length f) 0)
;;                    (display (localized-message tm:locale-number-separator) port)
;;                    (display f port)))))
;;    (cons #\h (lambda (date pad-with port)
;;                (display (date->string date "~b") port)))
;;    (cons #\H (lambda (date pad-with port)
;;                (display (tm:padding (srfi:date-hour date)
;;                                     pad-with 2)
;;                         port)))
;;    (cons #\I (lambda (date pad-with port)
;;                (let ((hr (srfi:date-hour date)))
;;                  (if (> hr 12)
;;                      (display (tm:padding (- hr 12)
;;                                           pad-with 2)
;;                               port)
;;                      (display (tm:padding hr
;;                                           pad-with 2)
;;                               port)))))
;;    (cons #\j (lambda (date pad-with port)
;;                (display (tm:padding (srfi:date-year-day date)
;;                                     pad-with 3)
;;                         port)))
;;    (cons #\k (lambda (date pad-with port)
;;                (display (tm:padding (srfi:date-hour date)
;;                                     #\0 2)
;;                         port)))
;;    (cons #\l (lambda (date pad-with port)
;;                (let ((hr (if (> (srfi:date-hour date) 12)
;;                              (- (srfi:date-hour date) 12) (srfi:date-hour date))))
;;                  (display (tm:padding hr  #\Space 2)
;;                           port))))
;;    (cons #\m (lambda (date pad-with port)
;;                (display (tm:padding (srfi:date-month date)
;;                                     pad-with 2)
;;                         port)))
;;    (cons #\M (lambda (date pad-with port)
;;                (display (tm:padding (srfi:date-minute date)
;;                                     pad-with 2)
;;                         port)))
;;    (cons #\n (lambda (date pad-with port)
;;                (newline port)))
;;    (cons #\N (lambda (date pad-with port)
;;                (display (tm:padding (date-nanosecond date)
;;                                     pad-with 9)
;;                         port)))
;;    (cons #\p (lambda (date pad-with port)
;;                (display (tm:locale-am/pm (srfi:date-hour date)) port)))
;;    (cons #\r (lambda (date pad-with port)
;;                (display (date->string date "~I:~M:~S ~p") port)))
;;    (cons #\s (lambda (date pad-with port)
;;                (display (time-second (date->time-utc date)) port)))
;;    (cons #\S (lambda (date pad-with port)
;;                (if (> (date-nanosecond date)
;;                       tm:nano)
;;                    (display (tm:padding (+ (srfi:date-second date) 1)
;;                                         pad-with 2)
;;                             port)
;;                    (display (tm:padding (srfi:date-second date)
;;                                         pad-with 2)
;;                             port))))
;;    (cons #\t (lambda (date pad-with port)
;;                (display #\Tab port)))
;;    (cons #\T (lambda (date pad-with port)
;;                (display (date->string date "~H:~M:~S") port)))
;;    (cons #\U (lambda (date pad-with port)
;;                (if (> (tm:days-before-first-week date 0) 0)
;;                    (display (tm:padding (+ (date-week-number date 0) 1)
;;                                         #\0 2) port)
;;                    (display (tm:padding (date-week-number date 0)
;;                                         #\0 2) port))))
;;    (cons #\V (lambda (date pad-with port)
;;                (display (tm:padding (date-week-number date 1)
;;                                     #\0 2) port)))
;;    (cons #\w (lambda (date pad-with port)
;;                (display (srfi:date-week-day date) port)))
;;    (cons #\x (lambda (date pad-with port)
;;                (display (date->string date (localized-message tm:locale-short-date-format)) port)))
;;    (cons #\X (lambda (date pad-with port)
;;                (display (date->string date (localized-message tm:locale-time-format)) port)))
;;    (cons #\W (lambda (date pad-with port)
;;                (if (> (tm:days-before-first-week date 1) 0)
;;                    (display (tm:padding (+ (date-week-number date 1) 1)
;;                                         #\0 2) port)
;;                    (display (tm:padding (date-week-number date 1)
;;                                         #\0 2) port))))
;;    (cons #\y (lambda (date pad-with port)
;;                (display (tm:padding (tm:last-n-digits 
;;                                      (srfi:date-year date) 2)
;;                                     pad-with
;;                                     2)
;;                         port)))
;;    (cons #\Y (lambda (date pad-with port)
;;                (display (srfi:date-year date) port)))
;;    (cons #\z (lambda (date pad-with port)
;;                (tm:tz-printer (date-zone-offset date) port)))
;;    (cons #\Z (lambda (date pad-with port)
;;                (tm:locale-print-time-zone date port)))
;;    (cons #\1 (lambda (date pad-with port)
;;                (display (date->string date "~Y-~m-~d") port)))
;;    (cons #\2 (lambda (date pad-with port)
;;                (display (date->string date "~k:~M:~S~z") port)))
;;    (cons #\3 (lambda (date pad-with port)
;;                (display (date->string date "~k:~M:~S") port)))
;;    (cons #\4 (lambda (date pad-with port)
;;                (display (date->string date "~Y-~m-~dT~k:~M:~S~z") port)))
;;    (cons #\5 (lambda (date pad-with port)
;;                (display (date->string date "~Y-~m-~dT~k:~M:~S") port)))
;;    ))


;; (define (tm:get-formatter char)
;;   (let ( (associated (assoc char tm:directives)) )
;;     (if associated (cdr associated) #f)))

;; (define (tm:date-printer date index format-string str-len port)
;;   (if (>= index str-len)
;;       (values)
;;       (let ( (current-char (string-ref format-string index)) )
;;         (if (not (char=? current-char #\~))
;;             (begin
;;               (display current-char port)
;;               (tm:date-printer date (+ index 1) format-string str-len port))
;;             (if (= (+ index 1) str-len) ; bad format string.
;;                 (tm:time-error 'tm:date-printer 'bad-date-format-string 
;;                                format-string)
;;                 (let ( (pad-char? (string-ref format-string (+ index 1))) )
;;                   (cond
;;                     ((char=? pad-char? #\-)
;;                      (if (= (+ index 2) str-len) ; bad format string.
;;                          (tm:time-error 'tm:date-printer 'bad-date-format-string 
;;                                         format-string)
;;                          (let ( (formatter (tm:get-formatter 
;;                                             (string-ref format-string
;;                                                         (+ index 2)))) )
;;                            (if (not formatter)
;;                                (tm:time-error 'tm:date-printer 'bad-date-format-string 
;;                                               format-string)
;;                                (begin
;;                                  (formatter date #f port)
;;                                  (tm:date-printer date (+ index 3)
;;                                                   format-string str-len port))))))
                    
;;                     ((char=? pad-char? #\_)
;;                      (if (= (+ index 2) str-len) ; bad format string.
;;                          (tm:time-error 'tm:date-printer 'bad-date-format-string 
;;                                         format-string)
;;                          (let ( (formatter (tm:get-formatter 
;;                                             (string-ref format-string
;;                                                         (+ index 2)))) )
;;                            (if (not formatter)
;;                                (tm:time-error 'tm:date-printer 'bad-date-format-string 
;;                                               format-string)
;;                                (begin
;;                                  (formatter date #\Space port)
;;                                  (tm:date-printer date (+ index 3)
;;                                                   format-string str-len port))))))
;;                     (else
;;                      (let ( (formatter (tm:get-formatter 
;;                                         (string-ref format-string
;;                                                     (+ index 1)))) )
;;                        (if (not formatter)
;;                            (tm:time-error 'tm:date-printer 'bad-date-format-string 
;;                                           format-string)
;;                            (begin
;;                              (formatter date #\0 port)
;;                              (tm:date-printer date (+ index 2)
;;                                               format-string str-len port))))))))))))


;; (define (date->string date [format-string "~c"])
;;   (unless (string? format-string)
;;     (raise-type-error 'date->string "string" 1 date format-string))
;;   (let ( (str-port (open-output-string)) )
;;     (tm:date-printer date 0 format-string (string-length format-string) str-port)
;;     (get-output-string str-port)))

;; (define (tm:char->int ch)
;;   (cond
;;     ((char=? ch #\0) 0)
;;     ((char=? ch #\1) 1)
;;     ((char=? ch #\2) 2)
;;     ((char=? ch #\3) 3)
;;     ((char=? ch #\4) 4)
;;     ((char=? ch #\5) 5)
;;     ((char=? ch #\6) 6)
;;     ((char=? ch #\7) 7)
;;     ((char=? ch #\8) 8)
;;     ((char=? ch #\9) 9)
;;     (else (tm:time-error 'bad-date-template-string
;;                          'digit-char
;;                          ch))))

;; ;; read an integer upto n characters long on port; upto -> #f if any length
;; (define (tm:integer-reader upto port)
;;   (define (accum-int port accum nchars)
;;     (let ((ch (peek-char port)))
;;       (if (or (eof-object? ch)
;;               (not (char-numeric? ch))
;;               (and upto (>= nchars  upto )))
;;           accum
;;           (accum-int port (+ (* accum 10) (tm:char->int (read-char port))) (+ nchars 1)))))
;;   (accum-int port 0 0))

;; (define (tm:make-integer-reader upto)
;;   (lambda (port)
;;     (tm:integer-reader upto port)))

;; ;; read an fractional integer upto n characters long on port; upto -> #f if any length
;; ;;
;; ;; The return value is normalized to upto decimal places. For example, if upto is 9 and 
;; ;; the string read is "123", the return value is 123000000.
;; (define (tm:fractional-integer-reader upto port)
;;   (define (accum-int port accum nchars)
;;     (let ((ch (peek-char port)))
;;       (if (or (eof-object? ch)
;;               (not (char-numeric? ch))
;;               (and upto (>= nchars  upto )))
;;           (* accum (expt 10 (- upto nchars)))
;;           (accum-int port (+ (* accum 10) (tm:char->int (read-char port))) (+ nchars 1)))))
;;   (accum-int port 0 0))

;; (define (tm:make-fractional-integer-reader upto)
;;   (lambda (port)
;;     (tm:fractional-integer-reader upto port)))

;; ;; read *exactly* n characters and convert to integer; could be padded
;; (define (tm:integer-reader-exact n port)
;;   (let ( (padding-ok #t) )
;;     (define (accum-int port accum nchars)
;;       (let ((ch (peek-char port)))
;;         (cond
;;           ((>= nchars n) accum)
;;           ((eof-object? ch) 
;;            (tm:time-error 'string->date 'bad-date-template-string 
;;                           "Premature ending to integer read."))
;;           ((char-numeric? ch)
;;            (set! padding-ok #f)
;;            (accum-int port (+ (* accum 10) (tm:char->int (read-char port)))
;;                       (+ nchars 1)))
;;           (padding-ok
;;            (read-char port)		; consume padding
;;            (accum-int port accum (+ nchars 1)))
;;           (else			; padding where it shouldn't be
;;            (tm:time-error 'string->date 'bad-date-template-string 
;;                           "Non-numeric characters in integer read.")))))
;;     (accum-int port 0 0)))


;; (define (tm:make-integer-exact-reader n)
;;   (lambda (port)
;;     (tm:integer-reader-exact n port)))

;; (define (tm:zone-reader port) 
;;   (let ( (offset 0) 
;;          (positive? #f) )
;;     (let ( (ch (read-char port)) )
;;       (when (eof-object? ch)
;;         (tm:time-error 'string->date 'bad-date-template-string
;;                        (list "Invalid time zone +/-" ch)))
;;       (if (or (char=? ch #\Z) (char=? ch #\z))
;;           0
;;           (begin
;;             (cond
;;               ((char=? ch #\+) (set! positive? #t))
;;               ((char=? ch #\-) (set! positive? #f))
;;               (else
;;                (tm:time-error 'string->date 'bad-date-template-string
;;                               (list "Invalid time zone +/-" ch))))
;;             (let ((ch (read-char port)))
;;               (when (eof-object? ch)
;;                 (tm:time-error 'string->date 'bad-date-template-string (list "Invalid time zone number" ch)))
;;               (set! offset (* (tm:char->int ch)
;;                               10 60 60)))
;;             (let ((ch (read-char port)))
;;               (unless (eof-object? ch)
;;                 ;; FIXME: non-existing values should be considered Zero instead of an error
;;                 ;; (tm:time-error 'string->date 'bad-date-template-string (list "Invalid time zone number" ch)))
;;                 (set! offset (+ offset (* (tm:char->int ch) 60 60)))))
;;             (let ((ch (read-char port)))
;;               (unless (eof-object? ch)
;;                 ;; FIXME: non-existing values should be considered Zero instead of an error
;;                 ;; (tm:time-error 'string->date 'bad-date-template-string (list "Invalid time zone number" ch)))
;;                 (set! offset (+ offset (* (tm:char->int ch) 10 60)))))
;;             (let ((ch (read-char port)))
;;               (unless (eof-object? ch)
;;                 ;; FIXME: non-existing values should be considered Zero instead of an error
;;                 ;; (tm:time-error 'string->date 'bad-date-template-string (list "Invalid time zone number" ch)))
;;                 (set! offset (+ offset (* (tm:char->int ch) 60)))))
;;             (if positive? offset (- offset)))))))

;; ;; looking at a char, read the char string, run thru indexer, return index
;; (define (tm:locale-reader port indexer)
;;   (let ( (string-port (open-output-string)) )
;;     (define (read-char-string)
;;       (let ((ch (peek-char port)))
;;         (if (char-alphabetic? ch)
;;             (begin (write-char (read-char port) string-port) 
;;                    (read-char-string))
;;             (get-output-string string-port))))
;;     (let* ( (str (read-char-string)) 
;;             (index (indexer str)) )
;;       (if index index (tm:time-error 'string->date
;;                                      'bad-date-template-string
;;                                      (list "Invalid string for " indexer))))))

;; (define (tm:make-locale-reader indexer)
;;   (lambda (port)
;;     (tm:locale-reader port indexer)))

;; (define (tm:make-char-id-reader char)
;;   (lambda (port)
;;     (if (char=? char (read-char port))
;;         char
;;         (tm:time-error 'string->date
;;                        'bad-date-template-string
;;                        "Invalid character match."))))

;; ;; A List of formatted read directives.
;; ;; Each entry is a list.
;; ;; 1. the character directive; 
;; ;; a procedure, which takes a character as input & returns
;; ;; 2. #t as soon as a character on the input port is acceptable
;; ;; for input,
;; ;; 3. a port reader procedure that knows how to read the current port
;; ;; for a value. Its one parameter is the port.
;; ;; 4. a action procedure, that takes the value (from 3.) and some
;; ;; object (here, always the date) and (probably) side-effects it.
;; ;; In some cases (e.g., ~A) the action is to do nothing
;; (define tm:read-directives 
;;   (let ( (ireader4 (tm:make-integer-reader 4))
;;          (ireader2 (tm:make-integer-reader 2))
;;          (fireader9 (tm:make-fractional-integer-reader 9))
;;          (ireaderf (tm:make-integer-reader #f))
;;          (eireader2 (tm:make-integer-exact-reader 2))
;;          (eireader4 (tm:make-integer-exact-reader 4))
;;          (locale-reader-abbr-weekday (tm:make-locale-reader
;;                                       tm:locale-abbr-weekday->index))
;;          (locale-reader-long-weekday (tm:make-locale-reader
;;                                       tm:locale-long-weekday->index))
;;          (locale-reader-abbr-month   (tm:make-locale-reader
;;                                       tm:locale-abbr-month->index))
;;          (locale-reader-long-month   (tm:make-locale-reader
;;                                       tm:locale-long-month->index))
;;          (char-fail (lambda (ch) #t))
;;          (do-nothing (lambda (val object) (values)))
;;          )
    
;;     (list
;;      (list #\~ char-fail (tm:make-char-id-reader #\~) do-nothing)
;;      (list #\a char-alphabetic? locale-reader-abbr-weekday do-nothing)
;;      (list #\A char-alphabetic? locale-reader-long-weekday do-nothing)
;;      (list #\b char-alphabetic? locale-reader-abbr-month
;;            (lambda (val object)
;;              (tm:set-date-month! object val)))
;;      (list #\B char-alphabetic? locale-reader-long-month
;;            (lambda (val object)
;;              (tm:set-date-month! object val)))
;;      (list #\d char-numeric? ireader2 (lambda (val object)
;;                                         (tm:set-date-day!
;;                                          object val)))
;;      (list #\e char-fail eireader2 (lambda (val object)
;;                                      (tm:set-date-day! object val)))
;;      (list #\h char-alphabetic? locale-reader-abbr-month
;;            (lambda (val object)
;;              (tm:set-date-month! object val)))
;;      (list #\H char-numeric? ireader2 (lambda (val object)
;;                                         (tm:set-date-hour! object val)))
;;      (list #\k char-fail eireader2 (lambda (val object)
;;                                      (tm:set-date-hour! object val)))
;;      (list #\m char-numeric? ireader2 (lambda (val object)
;;                                         (tm:set-date-month! object val)))
;;      (list #\M char-numeric? ireader2 (lambda (val object)
;;                                         (tm:set-date-minute!
;;                                          object val)))
;;      (list #\N char-numeric? fireader9 (lambda (val object)
;;                                          (tm:set-date-nanosecond! object val)))
;;      (list #\S char-numeric? ireader2 (lambda (val object)
;;                                         (tm:set-date-second! object val)))
;;      (list #\y char-fail eireader2 
;;            (lambda (val object)
;;              (tm:set-date-year! object (tm:natural-year val))))
;;      (list #\Y char-numeric? ireader4 (lambda (val object)
;;                                         (tm:set-date-year! object val)))
;;      (list #\z (lambda (c)
;;                  (or (char=? c #\Z)
;;                      (char=? c #\z)
;;                      (char=? c #\+)
;;                      (char=? c #\-)))
;;            tm:zone-reader (lambda (val object)
;;                             (tm:set-date-zone-offset! object val)))
;;      ; PLT-specific extension for 2- or 4-digit years:
;;      (list #\? char-numeric? ireader4 (lambda (val object)
;;                                         (tm:set-date-year! object (tm:natural-year val))))
;;      )))

;; (define (tm:string->date date index format-string str-len port template-string)
;;   (define (skip-until port skipper)
;;     (let ((ch (peek-char port)))
;;       (if (eof-object? ch)
;;           (tm:time-error 'string->date 'bad-date-format-string template-string)
;;           (unless (skipper ch)
;;             (read-char port)
;;             (skip-until port skipper)))))
;;   (if (>= index str-len)
;;       (begin 
;;         (values))
;;       (let ( (current-char (string-ref format-string index)) )
;;         (if (not (char=? current-char #\~))
;;             (let ((port-char (read-char port)))
;;               (when (or (eof-object? port-char)
;;                         (not (char=? current-char port-char)))
;;                 (tm:time-error 'string->date 'bad-date-format-string template-string))
;;               (tm:string->date date (+ index 1) format-string str-len port template-string))
;;             ;; otherwise, it's an escape, we hope
;;             (if (> (+ index 1) str-len)
;;                 (tm:time-error 'string->date 'bad-date-format-string template-string)
;;                 (let* ( (format-char (string-ref format-string (+ index 1)))
;;                         (format-info (assoc format-char tm:read-directives)) )
;;                   (if (not format-info)
;;                       (tm:time-error 'string->date 'bad-date-format-string template-string)
;;                       (begin
;;                         (let ((skipper (cadr format-info))
;;                               (reader  (caddr format-info))
;;                               (actor   (cadddr format-info)))
;;                           (skip-until port skipper)
;;                           (let ((val (reader port)))
;;                             (if (eof-object? val)
;;                                 (tm:time-error 'string->date 'bad-date-format-string template-string)
;;                                 (actor val date)))
;;                           (tm:string->date date (+ index 2) format-string  str-len port template-string))))))))))

;; (define (string->date input-string template-string)
;;   (define (tm:date-ok? date)
;;     (and (date-nanosecond date)
;;          (srfi:date-second date)
;;          (srfi:date-minute date)
;;          (srfi:date-hour date)
;;          (srfi:date-day date)
;;          (srfi:date-month date)
;;          (srfi:date-year date)
;;          (date-zone-offset date)))
;;   (let ( (newdate (srfi:make-date 0 0 0 0 #t #t #t (tm:local-tz-offset))) )
;;     (tm:string->date newdate
;;                      0
;;                      template-string
;;                      (string-length template-string)
;;                      (open-input-string input-string)
;;                      template-string)
;;     (if (tm:date-ok? newdate)
;;         newdate
;;         (tm:time-error 'string->date 'bad-date-format-string (list "Incomplete date read. " newdate template-string)))))
  
  

