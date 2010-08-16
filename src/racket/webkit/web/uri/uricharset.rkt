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

#lang racket/base

(provide encode-char
	 digit-char? hex-char? pchar? pct-encoded-char?
	 scheme-start-ch? scheme-tail-ch? sub-delim-char? unreserved-char?
	 unsafe-char?)

(require)

;; Amazon requires upcase letters in their signed URLs.
(define encode-char
  (lambda (ch)
    (string-append "%" (string-upcase (number->string (char->integer ch) 16)))))

(define encode-char?
  (lambda (ch)
    (or (unsafe-char? ch))))

(define digit-char?
  (lambda (ch)
    (and
     (char>=? ch #\0)
     (char<=? ch #\9))))

(define alphabet-char?
  (lambda (ch)
    (and
     (char-ci>=? ch #\a)
     (char-ci<=? ch #\z))))

(define hex-char?
  (lambda (ch)
    (or
     (digit-char? ch)
     (case (char-downcase ch)
       ((#\a #\b #\c #\d #\e #\f) #t)
       (else #f)))))

(define unreserved-char?
  (lambda (ch)
    (or
     (alphabet-char? ch)
     (digit-char? ch)
     (case ch
       ((#\. #\_ #\~ #\\ #\-) #t)
       (else #f)))))

(define reserved
  (lambda (ch)
    (or
     (general-delim-char? ch)
     (sub-delim-char? ch))))

;; rtf1138
(define unsafe-char?
  (lambda (ch)
    (case ch
      ((#\{ #\} #\| #\\ #\^ #\~ #\[ #\] #\`)
       #t)
      (else #f))))

(define general-delim-char?
  (lambda (ch)
    (case ch
      ((#\: #\/ #\? #\# #\[ #\] #\@) #t)
      (else #f))))

(define sub-delim-char?
  (lambda (ch)
    (case ch
      ((#\! #\$ #\& #\' #\( #\) #\* #\+ #\, #\; #\=) #t)
      (else #f))))

(define pct-encoded-char?
  (lambda (ch)
    (or
     (eq? ch #\%)
     (hex-char? ch))))

(define pchar?
  (lambda (ch)
    (or
     (unreserved-char? ch)
     (pct-encoded-char? ch)
     (sub-delim-char? ch)
     (case ch
       ((#\: #\@) #t)
       (else #f)))))

(define scheme-start-ch? alphabet-char?)

(define scheme-tail-ch?
  (lambda (ch)
    (or
     (scheme-start-ch? ch)
     (digit-char? ch)
     (case ch
       ((#\+ #\- #\.) #t)
       (else #f)))))
