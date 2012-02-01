;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Knozama's Amazon API Library
;; Copyright (C) 2007,2008,2009,2010  Raymond Paul Racine
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

#lang typed/racket/base

(provide Date Time 
	 current-date-string-iso-8601 iso-8601-date-string->date
	 current-date-string-rfc-2822
	 current-date local-current-date
	 date->time-utc
	 current-time
	 time< time<= time= time>= time>
	 time-difference
	 local-zone-offset)

(require/typed srfi/19
	       (opaque Date date?)
	       (opaque Time time?)
	       ((current-time current-time) (-> Time))
	       (time-difference (Date Date -> Integer))
	       ((time<? time<) (Time Time -> Boolean))
	       ((time<=? time<=) (Time Time -> Boolean))
	       ((time=? time=) (Time Time -> Boolean))
	       ((time>=? time>=) (Time Time -> Boolean))
	       ((time>? time>) (Time Time -> Boolean))
	       (date->time-utc (Date -> Time))
	       (date-zone-offset (Date -> Integer))
	       (current-date (Integer -> Date))
	       ((current-date local-current-date) (-> Date))
	       (string->date (String String -> Date))
	       (date->string (Date String -> String)))

(: local-zone-offset Integer)
(define local-zone-offset 
  (date-zone-offset (local-current-date)))

(: iso-8601-date-time-format String)
(define iso-8601-date-time-format "~Y-~m-~dT~H:~M:~S~z")

(: rfc2822-format String)
(define rfc2822-format "~a, ~d ~b ~Y ~T ~z")

(: current-date-string-rfc-2822 (-> String))
(define (current-date-string-rfc-2822)
  (date->string (local-current-date) rfc2822-format))

(: current-date-string-iso-8601 (Boolean -> String))
(define (current-date-string-iso-8601 zulu?)
  (let ((date (if zulu?
		  (current-date 0)
		  (local-current-date))))
    (date->string date iso-8601-date-time-format)))

(: iso-8601-date-string->date (String -> Date))
(define (iso-8601-date-string->date date-str)   
  (string->date date-str iso-8601-date-time-format))

;; (define (test)
;;   (pretty-print (iso-8601-date-string->date "2012-02-01T17:31:33.969Z"))
;;   (pretty-print (iso-8601-date-string->date "2012-02-01T17:31:33.969-0500")))


  ;; (define iso-8601-rgx 
  ;;   #px"(\\d{4})-(\\d{2})-(\\d{2})T(\\d{2}):(\\d{2}):(\\d{2})[.](\\d{3})(Z|-\\d{4})")

  ;; (pretty-print (regexp-match iso-8601-rgx "2012-02-01T17:31:33.969Z"))
  ;; (pretty-print (regexp-match iso-8601-rgx "2012-02-01T17:31:33.969-0500"))
  
  ;; "Hello")
  
;; (let ((matches (regexp-match iso-8601-rgx date-str)))
;;   (if (= (length matches) 9)
       



  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Current time as a rfc2822 date string
;; "Sat, 8 Sep 2007 18:19:20 -0400"
;; unit -> string
;; Note: Is relative local TZ and _not_ GMT
;;       which is ok.
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;(: rfc2822-date (-> Date))
;; (define (rfc2822-date) (current-date))
;;   (date-display-format 'rfc2822)
;;   (date->string (seconds->date (current-seconds)) #t))

