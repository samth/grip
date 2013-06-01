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

(provide
 Instant Instant? Instant-millis Instant-second
 Datetime Datetime? 
 Datetime-offset Datetime-year Datetime-month Datetime-day
 Datetime-week-day Datetime-hour Datetime-minute Datetime-second Datetime-milli
 Datetime-year-day
 Datetime->InstantUTC
 year-day days-before-first-week date-week-number)

(struct: Datetime ([offset : Integer]
		   [year : Integer]
		   [month : Integer]
		   [day : Integer]
		   [hour : Integer]
		   [minute : Integer]
		   [second : Integer]
		   [milli : Integer]) #:transparent)

(require
 (only-in "const.rkt"
	  sid tai-epoch-in-jd)
 (only-in "util.rkt"
	  time->julian-day-number
	  decode-julian-day-number
	  encode-julian-day-number))

;; The means of measuring time or timekeeping
;;(struct: Chronometry () #:transparent)

;; Instant in time in millis along with a Chronometry
;; Assumes a 64 bit build of Racket for the long haul.
(struct: Instant ([millis : Integer]) #:transparent)

(: Instant-second (Instant -> Integer))
(define (Instant-second instant)
  (inexact->exact (floor (/ (Instant-millis instant) 1000))))

(struct: InstantTAI Instant () #:transparent)
(struct: InstantUTC Instant () #:transparent)

(struct: Duration Instant () #:transparent)

;; ;; See date->string
;; ;;-- Miscellaneous Constants.
;; ;;-- only the tm:tai-epoch-in-jd might need changing if
;; ;;   a different epoch is used.

;;(define tm:nano (expt 10 9))
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


;; Now in millisecs TAI
(: now-tai (-> InstantTAI))
(define (now-tai)
  (InstantTAI (assert (inexact->exact (floor (current-inexact-milliseconds))) exact-integer?)))

(: now-utc (-> InstantUTC))
(define (now-utc)
  (instant-tai->utc (now-tai)))

(: now (-> InstantTAI))
(define now now-tai)

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

;; (define (tm:char-pos char str index len)
;;   (cond
;;     ((>= index len) #f)
;;     ((char=? (string-ref str index) char)
;;      index)
;;     (else
;;      (tm:char-pos char str (+ index 1) len))))

(define local-tz-offset
  (date-time-zone-offset (seconds->date (current-seconds))))

(: instant->datetime (InstantUTC (Option Integer) -> Datetime))
(define (instant->datetime instant offset)
  (let ((offset (if offset offset local-tz-offset)))
    (let-values (((secs date month year)
		  (decode-julian-day-number (time->julian-day-number (Instant-second instant) offset))))
      (let* ((hours    (quotient secs (* 60 60)))
	     (rem      (remainder secs (* 60 60)))
	     (minutes  (quotient rem 60))
	     (seconds  (remainder rem 60))
	     (millis   (remainder (Instant-millis instant) 1000)))
	(Datetime offset year month date hours minutes seconds millis)))))

(: instant-tai->datetime (InstantTAI (Option Integer) -> Datetime))
(define (instant-tai->datetime instant tz-offset)
  
  (: find (((Pair Exact-Positive-Integer Positive-Byte) -> Boolean) LeapSecondTable -> Boolean))
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
	(find (lambda: ((x : (Pair Exact-Positive-Integer Positive-Byte)))
		(= second (- (+ (car x) (cdr x)) 1)))
	      leap-second-table)
	#f))

  (: at-leap-sec-adjust (Datetime -> Datetime))
  (define (at-leap-sec-adjust date)
    (Datetime
     (Datetime-offset date)
     (Datetime-year date)
     (Datetime-month date)
     (Datetime-day date)
     (Datetime-hour date)
     (Datetime-minute date)
     60 ;; leap a second by adding a 60th second
     (Datetime-milli date)))
  
  (let ((dt (instant->datetime (instant-tai->utc instant) tz-offset)))
    (if (tai-before-leap-second? (Instant-second instant))
	(at-leap-sec-adjust dt)
	dt)))

(: instant-utc->date (InstantUTC (Option Integer) -> Datetime))
(define (instant-utc->date time tz-offset)
   (instant->datetime time tz-offset))

(: Datetime->InstantUTC (Datetime -> InstantUTC))
(define (Datetime->InstantUTC date)
  (let ((milli  (Datetime-milli date))
	(second (Datetime-second date))
	(minute (Datetime-minute date))
	(hour   (Datetime-hour date))
	(day    (Datetime-day date))
	(month  (Datetime-month date))
	(year   (Datetime-year date))
	(offset (Datetime-offset date)))
    (let ((jdays (- (encode-julian-day-number day month year)
		    tai-epoch-in-jd)))
      (InstantUTC
       (assert (+ (* (- jdays 1/2) 24 60 60)
		  (* hour 60 60)
		  (* minute 60)
		  second
		  milli
		  (- offset)) exact-integer?)))))



;; (define (date->time-tai d)
;;   (if (= (srfi:date-second d) 60)
;;       (subtract-duration! (time-utc->time-tai! (date->time-utc d)) (make-time time-duration 0 1))
;;       (time-utc->time-tai! (date->time-utc d))))

(: year-leap-year? (Integer -> Boolean))
(define (year-leap-year? year)
  (or (= (modulo year 400) 0)
      (and (= (modulo year 4) 0) (not (= (modulo year 100) 0)))))

(: datetime-leap-year? (Datetime -> Boolean))
(define (datetime-leap-year? datetime)
  (year-leap-year? (Datetime-year datetime)))

(: month-assoc (HashTable Integer Integer))
(define month-assoc (make-hasheqv '((0 . 0) (1 . 31)  (2 . 59)   (3 . 90)   (4 . 120) 
				    (5 . 151) (6 . 181)  (7 . 212)  (8 . 243)
				    (9 . 273) (10 . 304) (11 . 334))))

(: year-day (Integer Integer Integer -> Integer))
(define (year-day day month year)
  (let ((days-pr (hash-ref month-assoc (- month 1))))
    (if (and (year-leap-year? year) (> month 2))
        (+ day days-pr 1)
        (+ day days-pr))))

(: Datetime-year-day (Datetime -> Integer))
(define (Datetime-year-day datetime)
   (year-day (Datetime-day datetime) (Datetime-month datetime) (Datetime-year datetime)))

(: week-day (Integer Integer Integer -> Index))
(define (week-day day month year)
  (let* ((a (quotient (- 14 month) 12))
         (y (- year a))
         (m (+ month (* 12 a) -2)))
    (assert (modulo (+ day y (quotient y 4) (- (quotient y 100))
		       (quotient y 400) (quotient (* 31 m) 12))
		    7) index?)))

(: Datetime-week-day (Datetime -> Index))
(define (Datetime-week-day datetime)
   (week-day (Datetime-day datetime) (Datetime-month datetime) (Datetime-year datetime)))

(: days-before-first-week (Datetime Integer -> Index))
(define (days-before-first-week date day-of-week-starting-week)
  (let* ((first-day (Datetime (Datetime-offset date)
			      (Datetime-year date)
			      1 1 0 0 0 0))
	 (fdweek-day (Datetime-week-day first-day)))
    (modulo (- day-of-week-starting-week fdweek-day) 7)))

(: date-week-number (Datetime Integer -> Integer))
(define (date-week-number date day-of-week-starting-week)
  (quotient (- (Datetime-year-day date)
               (days-before-first-week date day-of-week-starting-week))
            7))

(: current-date ((Option Integer) -> Datetime))
(define (current-date offset)
  (let ((offset (if offset offset local-tz-offset)))
    (instant-tai->datetime (now-tai) offset)))

;; given a 'two digit' number, find the year within 50 years +/-
(: natural-year (Integer -> Integer))
(define (natural-year n)
  (let* ((current-year (Datetime-year (current-date #f)))
	 (current-century (* (quotient current-year 100) 100)))
    (cond
     ((>= n 100) n)
     ((<  n 0) n)
     ((<=  (- (+ current-century n) current-year) 50)
      (+ current-century n))
     (else
      (+ (- current-century 100) n)))))

;;;;;;;;;;;;;; FORMAT AND STUFF



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
  
  

