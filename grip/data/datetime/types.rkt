#lang typed/racket/base

(provide
 Tic InstantMaker
 (struct-out Interval)
 (struct-out Instant)
 (struct-out InstantTAI)
 (struct-out InstantUTC)
 (struct-out JulianDay)
 (struct-out ModifiedJulianDay)
 (struct-out Date)
 (struct-out Time)
 (struct-out Datetime))

;; 1 Tic == 1 millisecond.
;; Instant in time in millis along with a Chronometry.
;; Assumes a 64 bit build of Racket for the long haul.
(define-type Tic Integer)
(define-type Chronometry (U 'TAI 'UTC))
(define-type (InstantMaker X) (Tic -> X))

(struct: Instant ([tics : Tic]) #:transparent)
(struct: InstantTAI Instant () #:transparent)
(struct: InstantUTC Instant () #:transparent)

(struct: ChronometricInstant Instant ([chronometry : Chronometry]) #:transparent)

(struct: Interval ([tics : Tic]) #:transparent)

(struct: JulianDay ([day : Exact-Rational]) #:transparent)
(struct: ModifiedJulianDay ([day : Exact-Rational]) #:transparent)

(struct: Date ([year   : Integer]
	       [month  : Integer]
	       [day    : Integer]) #:transparent)

(struct: Time ([offset : Integer]
	       [hour   : Integer]
	       [minute : Integer]
	       [second : Integer]
	       [milli  : Integer]) #:transparent)

(struct: Datetime ([date : Date]
		   [time : Time]) #:transparent)


;; (define-type Tics Integer)
;; (define-type (ICtor X) (Tics -> X))
;; (struct: I ([millis : Tics]))
;; (struct: TAI I ())
;; (struct: UTC I ())
;; (struct: Interval I ())

;; (: future-instant (All (X) I Interval (ICtor X) -> X))
;; (define (future-instant i d c)
;;   (c (+ (I-millis i) (I-millis d))))


;; (define: f-tai : TAI (future-instant (TAI 0) (Interval 100) TAI))
