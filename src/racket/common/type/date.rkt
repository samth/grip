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

(provide current-time-iso-8601
	 current-time-rfc-2822)

(require/typed srfi/19
	       (opaque Date19 date?)
	       (current-date (-> Date19))
	       (date->string (Date19 String -> String)))

(: iso-8601-date-time-format String)
(define iso-8601-date-time-format "~Y-~m-~dT~H:~M:~S~z")

(: rfc2822-format String)
(define rfc2822-format "~a, ~d ~b ~Y ~T ~z")

(: current-time-rfc-2822 (-> String))
(define (current-time-rfc-2822)
  (date->string (current-date) rfc2822-format))

(define (current-time-iso-8601)
  (date->string (current-date) iso-8601-date-time-format))

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

