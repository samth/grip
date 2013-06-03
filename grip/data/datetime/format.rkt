#lang typed/racket/base

(require
 racket/fixnum
 racket/match
 (only-in "format-util.rkt"
	  localized-message 
	  last-n-digits
	  padding)
 (only-in "types.rkt"	  
	  Instant Instant? 
	  Datetime Datetime? Datetime-date Datetime-time
	  Date Date? Date-day Date-month Date-year
	  Time Time? Time-offset Time-hour Time-minute Time-second Time-milli
	  )
 (only-in "convert.rkt"
	  Datetime->InstantUTC)
 (only-in "date.rkt"
	  week-day year-day days-before-first-week week-number)
 (only-in "instant.rkt"
	  instant-seconds)) 

(define locale-pm 'pm)
(define locale-am 'am)

(define locale-date-time-format 'date-time)

(define locale-short-date-format 'date)
(define locale-time-format 'time)
;; (define tm:iso-8601-date-time-format 'iso8601)

(define locale-number-separator 'separator)

(define locale-abbr-weekday-vector (vector 'sun 'mon 'tue 'wed 'thu 'fri 'sat))
(define locale-long-weekday-vector (vector 'sunday 'monday 'tuesday 'wednesday
					   'thursday 'friday 'saturday))
;; note empty string in 0th place. 
(define locale-abbr-month-vector (vector 'jan 'feb 'mar
					 'apr 'may 'jun 'jul
					 'aug 'sep 'oct 'nov
					 'dec))

(define locale-long-month-vector (vector 'january 'february
					 'march 'april 'may
					 'june 'july 'august
					 'september 'october
					 'november 'december)) 

(: locale-am/pm (Integer -> String))
(define (locale-am/pm hr)
  (localized-message (if (> hr 11) locale-pm locale-am)))

(: locale-abbr-weekday (Index -> String))
(define (locale-abbr-weekday n) 
  (localized-message (vector-ref locale-abbr-weekday-vector n)))

(: locale-long-weekday (Index -> String))
(define (locale-long-weekday n)
  (localized-message (vector-ref locale-long-weekday-vector n)))

(: locale-abbr-month (Index -> String))
(define (locale-abbr-month n)
  (localized-message (vector-ref locale-abbr-month-vector (- n 1))))

(: locale-long-month (Index -> String))
(define (locale-long-month n)
  (localized-message (vector-ref locale-long-month-vector (- n 1))))

;; do nothing. 
;; Your implementation might want to do something...
(: locale-print-time-zone (Datetime Output-Port -> Void))
(define (locale-print-time-zone date port)
  (void))

(: vector-find (String (Vectorof Symbol) (String String -> Boolean) -> (Option Index)))
(define (vector-find needle haystack comparator)
  (: len Index)
  (define len (vector-length haystack))
  (: vector-find-int (Fixnum -> (Option Index)))
  (define (vector-find-int index)
    (cond
     ((>= index len) #f)
     ((comparator needle (localized-message (vector-ref haystack index))) (assert (fx+ index 1) index?))
     (else (vector-find-int (fx+ index 1)))))

  (vector-find-int 0))


					; return a string representing the decimal expansion of the fractional 
					; portion of a number, limited by a specified precision
(: decimal-expansion (Real Fixnum -> String))
(define (decimal-expansion r precision)
  (let: loop : String ([num : Real (- r (round r))]
		       [p : Fixnum precision])
	(if (or (= p 0) (= num 0))
	    ""
	    (let* ([num-times-10 (* 10 num)]
		   [round-num-times-10 (round num-times-10)]) 
	      (string-append (number->string (inexact->exact round-num-times-10))
			     (loop (- num-times-10 round-num-times-10) (fx- p 1)))))))

(: locale-abbr-weekday->index (String -> (Option Index)))
(define (locale-abbr-weekday->index string)
  (vector-find string locale-abbr-weekday-vector string=?))

(: locale-long-weekday->index (String -> (Option Index)))
(define (locale-long-weekday->index string)
  (vector-find string locale-long-weekday-vector string=?))

(: locale-abbr-month->index (String -> (Option Index)))
(define (locale-abbr-month->index string)
  (vector-find string locale-abbr-month-vector string=?))

(: locale-long-month->index (String -> (Option Index)))
(define (locale-long-month->index string)
  (vector-find string locale-long-month-vector string=?))


(: tz-printer (Integer Output-Port -> Void))
(define (tz-printer offset port)
  (display (cond [(= offset 0) "Z"]
		 [else (let ([sign (cond [(negative? offset) "-"]
					 [else               "+"])]
			     [hours (abs (quotient offset (* 60 60)))]
			     [minutes (abs (quotient (remainder offset (* 60 60)) 60))])
			 (string-append sign 
					(padding hours #\0 2) 
					(padding minutes #\0 2)))])
	   port))


(: date-printer (Datetime Fixnum String Index Output-Port -> Void))
(define (date-printer date index format-string str-len port)
  (if (>= index str-len)
      (void)
      (let ( (current-char (string-ref format-string index)) )
        (if (not (char=? current-char #\~))
            (begin
              (display current-char port)
              (date-printer date (fx+ index 1) format-string str-len port))
            (if (= (fx+ index 1) str-len) ; bad format string.
                (error 'date-printer "bad-date-format-string" format-string)
                (let ( (pad-char? (string-ref format-string (fx+ index 1))) )
                  (cond
		   ((char=? pad-char? #\-)
		    (if (= (fx+ index 2) str-len) ; bad format string.
			(error 'date-printer "bad-date-format-string"
			       format-string)
			(let ( (formatter (get-formatter 
					   (string-ref format-string
						       (fx+ index 2)))) )
			  (if (not formatter)
			      (error 'date-printer "bad-date-format-string" format-string)
			      (begin
				(formatter date #f port)
				(date-printer date (fx+ index 3)
					      format-string str-len port))))))
		   
		   ((char=? pad-char? #\_)
		    (if (= (fx+ index 2) str-len) ; bad format string.
			(error 'date-printer "bad-date-format-string" format-string)
			(let ( (formatter (get-formatter 
					   (string-ref format-string
						       (fx+ index 2)))) )
			  (if (not formatter)
			      (error 'date-printer "bad-date-format-string" format-string)
			      (begin
				(formatter date #\Space port)
				(date-printer date (fx+ index 3)
					      format-string str-len port))))))
		   (else
		    (let ( (formatter (get-formatter 
				       (string-ref format-string
						   (fx+ index 1)))) )
		      (if (not formatter)
			  (error 'date-printer "bad-date-format-string" format-string)
			  (begin
			    (formatter date #\0 port)
			    (date-printer date (fx+ index 2)
					  format-string str-len port))))))))))))


(: date->string (Datetime (Option String) -> String))
(define (date->string date format-string)
  (let ((format-string (if format-string format-string "~c")))
    (unless (string? format-string)
	    (raise-type-error 'date->string "string" 1 date format-string))
    (let ( (str-port (open-output-string)) )
      (date-printer date 0 format-string (string-length format-string) str-port)
      (get-output-string str-port))))

(define-type DatetimeFormatter (Datetime (Option Char) Output-Port -> Void))

;; A table of output formatting directives.
;; the first time is the format char.
;; the second is a procedure that takes the date, a padding character
;; (which might be #f), and the output port.
(: directives (Listof (Pair Char DatetimeFormatter)))
(define directives 
  (list
   (cons #\~ (lambda: ([date : Datetime] [pad-with : (Option Char)] [port : Output-Port]) 
		      (display #\~ port)))   
   (cons #\a (lambda: ([date : Datetime] [pad-with : (Option Char)] [port : Output-Port])
		      (display (locale-abbr-weekday (week-day (Datetime-date date)))
			       port)))
   (cons #\A (lambda: ([date : Datetime] [pad-with : (Option Char)] [port : Output-Port])
		      (display (locale-long-weekday (week-day (Datetime-date date)))
			       port)))
   (cons #\b (lambda: ([date : Datetime] [pad-with : (Option Char)] [port : Output-Port])
		      (display (locale-abbr-month (assert (Date-month (Datetime-date date)) index?))
			       port)))
   (cons #\B (lambda: ([date : Datetime] [pad-with : (Option Char)] [port : Output-Port])
		      (display (locale-long-month (assert (Date-month (Datetime-date date)) index?))
			       port)))
   (cons #\c (lambda: ([date : Datetime] [pad-with : (Option Char)] [port : Output-Port])
		      (display (date->string date (localized-message locale-date-time-format)) port)))
   (cons #\d (lambda: ([date : Datetime] [pad-with : (Option Char)] [port : Output-Port])
		      (display (padding (Date-day (Datetime-date date))
					#\0 2)
			       port)))
   (cons #\D (lambda: ([date : Datetime] [pad-with : (Option Char)] [port : Output-Port])
		      (display (date->string date "~m/~d/~y") port)))
   (cons #\e (lambda: ([date : Datetime] [pad-with : (Option Char)] [port : Output-Port])
		      (display (padding (Date-day (Datetime-date date))
					#\Space 2)
			       port)))
   (cons #\f (lambda: ([date : Datetime] [pad-with : (Option Char)] [port : Output-Port])
		      (display (padding (Time-second (Datetime-time date)) pad-with 2)
			       port)
		      (let ([f (decimal-expansion 0 6)])
			(when (> (string-length f) 0)
			      (display (localized-message locale-number-separator) port)
			      (display f port)))))
   (cons #\h (lambda: ([date : Datetime] [pad-with : (Option Char)] [port : Output-Port])
		      (display (date->string date "~b") port)))
   (cons #\H (lambda: ([date : Datetime] [pad-with : (Option Char)] [port : Output-Port])
		      (display (padding (Time-hour (Datetime-time date))
					pad-with 2)
			       port)))
   (cons #\I (lambda: ([date : Datetime] [pad-with : (Option Char)] [port : Output-Port])
		      (let ((hr (Time-hour (Datetime-time date))))
			(if (> hr 12)
			    (display (padding (- hr 12)
					      pad-with 2)
				     port)
			    (display (padding hr
					      pad-with 2)
				     port)))))
   (cons #\j (lambda: ([date : Datetime] [pad-with : (Option Char)] [port : Output-Port])
		      (display (padding (year-day (Datetime-date date))
					pad-with 3)
			       port)))
   (cons #\k (lambda: ([date : Datetime] [pad-with : (Option Char)] [port : Output-Port])
		      (display (padding (Time-hour (Datetime-time date))
					#\0 2)
			       port)))
   (cons #\l (lambda: ([date : Datetime] [pad-with : (Option Char)] [port : Output-Port])
		      (let ((hr (if (> (Time-hour (Datetime-time date)) 12)
				    (- (Time-hour (Datetime-time date)) 12) (Time-hour (Datetime-time date)))))
			(display (padding hr  #\Space 2)
				 port))))
   (cons #\ (lambda: ([date : Datetime] [pad-with : (Option Char)] [port : Output-Port])
		     (display (padding (Date-month (Datetime-date date))
				       pad-with 2)
			      port)))
   (cons #\M (lambda: ([date : Datetime] [pad-with : (Option Char)] [port : Output-Port])
		      (display (padding (Time-minute (Datetime-time date))
					pad-with 2)
			       port)))
   (cons #\n (lambda: ([date : Datetime] [pad-with : (Option Char)] [port : Output-Port])
		      (newline port)))
   (cons #\N (lambda: ([date : Datetime] [pad-with : (Option Char)] [port : Output-Port])
		      (display (padding 0 pad-with 9)
			       port)))
   (cons #\p (lambda: ([date : Datetime] [pad-with : (Option Char)] [port : Output-Port])
		      (display (locale-am/pm (Time-hour (Datetime-time date))) port)))
   (cons #\r (lambda: ([date : Datetime] [pad-with : (Option Char)] [port : Output-Port])
		      (display (date->string date "~I:~M:~S ~p") port)))
   (cons #\s (lambda: ([date : Datetime] [pad-with : (Option Char)] [port : Output-Port])
		      (display (instant-seconds (Datetime->InstantUTC date)) port)))
   (cons #\S (lambda: ([date : Datetime] [pad-with : (Option Char)] [port : Output-Port])
		      (display (padding (Time-second (Datetime-time date))
					pad-with 2)
			       port)))
   (cons #\t (lambda: ([date : Datetime] [pad-with : (Option Char)] [port : Output-Port])
		      (display #\Tab port)))
   (cons #\T (lambda: ([date : Datetime] [pad-with : (Option Char)] [port : Output-Port])
		      (display (date->string date "~H:~M:~S") port)))
   (cons #\U (lambda: ([date : Datetime] [pad-with : (Option Char)] [port : Output-Port])
		      (if (> (days-before-first-week (Datetime-date date) 0) 0)
			  (display (padding (+ (week-number (Datetime-date date) 0) 1)
					    #\0 2) port)
			  (display (padding (week-number (Datetime-date date) 0)
					    #\0 2) port))))
   (cons #\V (lambda: ([date : Datetime] [pad-with : (Option Char)] [port : Output-Port])
		      (display (padding (week-number (Datetime-date date) 1)
					#\0 2) port)))
   (cons #\w (lambda: ([date : Datetime] [pad-with : (Option Char)] [port : Output-Port])
		      (display (week-day (Datetime-date date)) port)))
   (cons #\x (lambda: ([date : Datetime] [pad-with : (Option Char)] [port : Output-Port])
		      (display (date->string date (localized-message locale-short-date-format)) port)))
   (cons #\X (lambda: ([date : Datetime] [pad-with : (Option Char)] [port : Output-Port])
		      (display (date->string date (localized-message locale-time-format)) port)))
   (cons #\W (lambda: ([date : Datetime] [pad-with : (Option Char)] [port : Output-Port])
		      (if (> (days-before-first-week (Datetime-date date) 1) 0)
			  (display (padding (+ (week-number (Datetime-date date) 1) 1)
					    #\0 2) port)
			  (display (padding (week-number (Datetime-date date) 1)
					    #\0 2) port))))
   (cons #\y (lambda: ([date : Datetime] [pad-with : (Option Char)] [port : Output-Port])
		      (display (padding (last-n-digits 
					 (Date-year (Datetime-date date)) 2)
					pad-with
					2)
			       port)))
   (cons #\Y (lambda: ([date : Datetime] [pad-with : (Option Char)] [port : Output-Port])
		      (display (Date-year (Datetime-date date)) port)))
   (cons #\z (lambda: ([date : Datetime] [pad-with : (Option Char)] [port : Output-Port])
		      (tz-printer (Time-offset (Datetime-time date)) port)))
   (cons #\Z (lambda: ([date : Datetime] [pad-with : (Option Char)] [port : Output-Port])
		      (locale-print-time-zone date port)))
   (cons #\1 (lambda: ([date : Datetime] [pad-with : (Option Char)] [port : Output-Port])
		      (display (date->string date "~Y-~m-~d") port)))
   (cons #\2 (lambda: ([date : Datetime] [pad-with : (Option Char)] [port : Output-Port])
		      (display (date->string date "~k:~M:~S~z") port)))
   (cons #\3 (lambda: ([date : Datetime] [pad-with : (Option Char)] [port : Output-Port])
		      (display (date->string date "~k:~M:~S") port)))
   (cons #\4 (lambda: ([date : Datetime] [pad-with : (Option Char)] [port : Output-Port])
		      (display (date->string date "~Y-~m-~dT~k:~M:~S~z") port)))
   (cons #\5 (lambda: ([date : Datetime] [pad-with : (Option Char)] [port : Output-Port])
		      (display (date->string date "~Y-~m-~dT~k:~M:~S") port)))))

(: get-formatter (Char -> (Option DatetimeFormatter)))
(define (get-formatter char)
  (let ((associated (assoc char directives)) )
    (if associated (cdr associated) #f)))
