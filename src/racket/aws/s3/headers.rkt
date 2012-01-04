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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; #lang typed/racket/base

;; (provide date-header
;; 	 content-type
;; 	 content-length
;; 	 content-md5)
  
;;   ;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;   ;; Makers of http headers
;;   ;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (: make-header (String String -> String))  
;; (define (make-header key value)
;;   (string-append key ": " value))

;; (: date-header (String -> String))
;; (define (date-header date)
;;   (make-header "Date" date))

;; (: content-type (String -> String))
;; (define (content-type mime)
;;   (make-header "Content-Type" mime))

;; (: content-length (Integer -> String))
;; (define (content-length len)
;;   (make-header "Content-Length" (number->string len)))

;; (: content-md5 (String -> String))
;; (define (content-md5 md5)
;;   (make-header "Content-MD5" md5))
