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

(provide:
 [year-leap-year? (Integer -> Boolean)]
 [date-leap-year? (Date -> Boolean)]
 [year-day (Date -> Integer)]
 [week-day (Date -> Index)]
 [week-number (Date Integer -> Integer)]
 [days-before-first-week (Date Integer -> Index)]
 [natural-year (Integer -> Integer)])

(require
 racket/match
 (only-in "const.rkt"
	  sid tai-epoch-in-jd days-in-week)
 (only-in "types.rkt"
	  Date Date-year Time Datetime)
 (only-in "convert.rkt"
	  current-date))

(: month-assoc (HashTable Integer Integer))
(define month-assoc (make-hasheqv 
		     '((0 . 0)    (1 . 31)  (2 . 59)  (3 . 90)  (4 . 120)
		       (5 . 151)  (6 . 181) (7 . 212) (8 . 243) (9 . 273)
		       (10 . 304) (11 . 334))))

(: year-leap-year? (Integer -> Boolean))
(define (year-leap-year? year)
  (or (= (modulo year 400) 0)
      (and (= (modulo year 4) 0) (not (= (modulo year 100) 0)))))

(: date-leap-year? (Date -> Boolean))
(define (date-leap-year? date)
  (match date
	 ((Date year _ _)
	  (year-leap-year? year))))

(: year-day (Date -> Integer))
(define (year-day date)

  (: calc-year-day (Integer Integer Integer -> Integer))
  (define (calc-year-day day month year)
    (let ((days-pr (hash-ref month-assoc (- month 1))))
      (if (and (year-leap-year? year) (> month 2))
	  (+ day days-pr 1)
	  (+ day days-pr))))
  
  (match date
	 ((Date year month day)
	  (calc-year-day year month day))))

(: calc-week-day (Integer Integer Integer -> Index))
(define (calc-week-day day month year)
  (let* ((a (quotient (- 14 month) 12))
	 (y (- year a))
	 (m (+ month (* 12 a) -2)))
    (assert (modulo (+ day y (quotient y 4) (- (quotient y 100))
		       (quotient y 400) (quotient (* 31 m) 12))
		    days-in-week) index?)))

(: week-day (Date -> Index))
(define (week-day datetime)
  (match datetime
	 ((Date year month day)
	  (calc-week-day year month day))))

(: days-before-first-week (Date Integer -> Index))
(define (days-before-first-week date day-of-week-starting-week)
  (let ((fdweek-day (calc-week-day 1 1 (Date-year date))))
    (modulo (- day-of-week-starting-week fdweek-day) 7)))

(: week-number (Date Integer -> Integer))
(define (week-number date day-of-week-starting-week)
  (quotient (- (year-day date) 
	       (days-before-first-week date day-of-week-starting-week))
	    days-in-week))

;; given a 'two digit' number, find the year within 50 years +/-
(: natural-year (Integer -> Integer))
(define (natural-year n)
  (let* ((current-year (match (current-date)
			      ((Datetime (Date year _ _) _) year)))
	 (current-century (* (quotient current-year 100) 100)))
    (cond
     ((>= n 100) n)
     ((<  n 0) n)
     ((<=  (- (+ current-century n) current-year) 50)
      (+ current-century n))
     (else
      (+ (- current-century 100) n)))))

;; Shite and Stuff

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


;; (define (tm:char-pos char str index len)
;;   (cond
;;     ((>= index len) #f)
;;     ((char=? (string-ref str index) char)
;;      index)
;;     (else
;;      (tm:char-pos char str (+ index 1) len))))


;; (define (date->time-tai d)
;;   (if (= (srfi:date-second d) 60)
;;       (subtract-duration! (time-utc->time-tai! (date->time-utc d)) (make-time time-duration 0 1))
;;       (time-utc->time-tai! (date->time-utc d))))
