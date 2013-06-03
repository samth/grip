#lang typed/racket

(provide
 LeapMilliSecondTable)

(provide:
 [leap-millisecond-table LeapMilliSecondTable]
 [read-tai-utc-data (String -> (Listof Any))]
 [leap-second-delta (Integer -> Nonnegative-Integer)]
 [leap-second-neg-delta (Positive-Integer -> Nonnegative-Integer)])

(require
 (only-in "const.rkt"
	  sid tai-epoch-in-jd))

(define-type LeapMilliSecondTable (Listof (Pair Nonnegative-Integer Nonnegative-Integer)))

;; Prints a pastable LeapSecondTable.
;; See ftp://maia.usno.navy.mil/ser7/tai-utc.dat and update as necessary.
;; Parses and prints a pastable table of leapseconds from the above ftp site.
(: read-tai-utc-data (String -> (Listof Any)))
(define (read-tai-utc-data filename)
  (: convert-jd (Number -> Number))
  (define (convert-jd jd)
    (* (* (- (inexact->exact jd)
	     tai-epoch-in-jd) sid) 1000))
  (: convert-sec (Number -> Integer))
  (define (convert-sec sec)
    (assert (* (inexact->exact sec) 1000) exact-integer?))
  (let ((port (open-input-file filename))
	(table '()) )
    (let loop ((line (read-line port)))
      (unless (eq? line eof)
	      (let* ((data (assert (read (open-input-string (string-append "(" line ")"))) list?))
		     (year (assert (car data) real?))
		     (jd   (assert (cadddr (cdr data)) real?))
		     (secs (assert (cadddr (cdddr data)) real?)) )
		(when (>= year 1972)
		      (set! table (cons (cons (convert-jd jd)
					      (convert-sec secs)) table)))
		(loop (read-line port)))))
    table))

(: leap-millisecond-table LeapMilliSecondTable)
(define leap-millisecond-table
  '((1341100800000 . 35000)
    (1230768000000 . 34000)
    (1136073600000 . 33000)
    (915148800000  . 32000)
    (867715200000  . 31000)
    (820454400000  . 30000)
    (773020800000  . 29000)
    (741484800000  . 28000)
    (709948800000  . 27000)
    (662688000000  . 26000)
    (631152000000  . 25000)
    (567993600000  . 24000)
    (489024000000  . 23000)
    (425865600000  . 22000)
    (394329600000  . 21000)
    (362793600000  . 20000)
    (315532800000  . 19000)
    (283996800000  . 18000)
    (252460800000  . 17000)
    (220924800000  . 16000)
    (189302400000  . 15000)
    (157766400000  . 14000)
    (126230400000  . 13000)
    (94694400000   . 12000)
    (78796800000   . 11000)
    (63072000000   . 10000)))

;; no UTC leap seconds prior to this moment in time.
(: no-leap-instant Integer)
(define no-leap-instant (* (- 1972 1970) 365 sid 1000))

;; Given milliseconds UTC.
;; Number of leap seconds between TAI and UTC, i.e. TAI - UTC
(: leap-second-delta (Integer -> Nonnegative-Integer))
(define (leap-second-delta utc-milliseconds)
  (if (< utc-milliseconds no-leap-instant)
      0
      (do: : Nonnegative-Integer ([table : LeapMilliSecondTable leap-millisecond-table table])
	   ((>= utc-milliseconds (caar table)) (cdar table)))))

;; going from tai seconds to utc seconds ...
(: leap-second-neg-delta (Exact-Positive-Integer -> Nonnegative-Integer))
(define (leap-second-neg-delta tai-seconds)
  (if (< tai-seconds no-leap-instant)
      0
      (let: loop : Nonnegative-Integer ([table : LeapMilliSecondTable leap-millisecond-table])
	    (cond
	     ((null? table) 0)
	     ((<= (cdar table)
		  (- tai-seconds (caar table)))
	      (cdar (assert table pair?)))
	     (else (loop (cdr table)))))))
