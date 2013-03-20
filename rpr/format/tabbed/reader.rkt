;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Ray Racine's Typed Racket Library
;; Copyright (C) 2007-2013  Raymond Paul Racine
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

(provide: 
 [string-field  (Input-Port -> String)]
 [number-field  (Input-Port -> Number)]
 [integer-field (Input-Port -> Integer)]
 [float-field   (Input-Port -> Float)])

(: string-field (Input-Port -> String))
(define (string-field inp)
  (define outp (open-output-string))
  (let loop ((ch (read-char inp)))
    (if (eof-object? ch)
	(get-output-string outp)
	(if (char=? ch #\tab)
	    (get-output-string outp)
	    (begin
	      (display ch outp)
	      (loop (read-char inp)))))))
  
(: number-field (Input-Port -> Number))
(define (number-field inp)
  (let ((n (read inp)))
    (if (number? n)
	n
	(error "Expected Number value: ~s" n))))

(: integer-field (Input-Port -> Integer))
(define (integer-field inp)
  (let ((v (read inp)))
    (if (exact-integer? v)
	v
	(error "Expected Integer value: ~s" v))))

(: float-field (Input-Port -> Float))
(define (float-field inp)
  (let ((v (read inp)))
    (cond
     ((real? v)
      (real->double-flonum v))
     ((eof-object? v)
      +nan.0)
     (else      
      (error 'float-field "Expected Float value: ~s" v)))))
