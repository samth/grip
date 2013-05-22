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

#lang typed/racket

(require 
 (for-syntax
  syntax/parse))

(: double-quote? (Char -> Boolean))
(define (double-quote? ch)
  (char=? #\" ch))

(: comma-delimiter? (Char -> Boolean))
(define (comma-delimiter? ch)
  (char=? #\, ch))

(: eol-delimiter? (Char -> Boolean))
(define (eol-delimiter? ch)
  (or (char=? #\linefeed ch)
      (char=? #\newline ch)))

(: eol? (Char Input-Port -> Boolean))
(define (eol? ch inp)
  (let ((rs  (eol-delimiter? ch)))
    (let ((ch (peek-char inp)))
      (and (char? ch)
	   (eol-delimiter? ch))
      (read-char inp))
    rs))

(define-syntax (read-until stx)
  (syntax-parse stx
		((_ in-port:id stop:id)
		 #'(let loop ()
		     (let ((ch (peek-char in-port)))
		       (if (eof-object? ch)
			   (void)
			   (if (stop ch)
			       (void)
			       (begin
				 (read-char in-port)
				 (loop)))))))))

(: toss-whitespace (Input-Port -> Void))
(define (toss-whitespace inp)
  (read-until inp char-whitespace?))

;; A string field has significant whitespace between ',' delimiters.
;; It may be quoted.
;; Internal quotes must be double quoted.
;; e.g. "    field with white-space with a ""quoted"" internal value     "
(: read-string-value (Input-Port -> String))
(define (read-string-value inp)
  (define outp (open-output-string))

  (define quoted (let ((ch (peek-char inp)))
		   (and (char? ch)
			(double-quote? ch))))

  (let: loop : String ((ch : (U Char EOF) (read-char inp)) (in-dquote : Boolean #f))
	(if (eof-object? ch)
	    (if (and quoted in-dquote)
		(error "Invalid CSV EOF with un-opened quote: ~s" (get-output-string outp))
		(get-output-string outp))
	    (cond
	     ((or (comma-delimiter? ch)
		  (eol? ch inp))
	      (if (and quoted in-dquote)
		  (error "Invalid CSV EOF with un-opened quote: ~s" (get-output-string outp))
		  (get-output-string outp)))
	     ((double-quote? ch)
	      (if quoted
		  (get-output-string outp)
		  (if in-dquote
		      (let ((ch (peek-char inp)))
			(if (and (char? ch)
				 (double-quote? ch))
			    (begin
			      (write ch outp)
			      (read-char inp) ;; toss second " in ""
			      (loop (read-char inp) (not in-dquote)))
			    (error "Invalid CSV EOF with un-opened quote: ~s" 
				   (get-output-string outp))))
		      (error "Invalid CSV EOF with un-opened quote: ~s" (get-output-string outp)))))
	     (else
	      (write ch outp)
	      (loop (read-char inp) in-dquote))))))

(: read-number-string (Input-Port -> String))
(define (read-number-string inp) 
  (define outp (open-output-string))
  (let: loop : String ((ch : (U Char EOF) (read-char inp)))
	(if (eof-object? ch)
	    (get-output-string outp)
	    (cond
	     ((or (char-numeric? ch)
		  (char=? #\. ch))
	      (write ch outp)
	      (loop (read-char inp)))
	     ((or (eol? ch inp)
		  (comma-delimiter? ch))
	      (get-output-string outp))
	     (else
	      (error "Expected integer field: ~s" (get-output-string outp)))))))

(: s->i (String -> Integer))
(define (s->i s)
  (let ((n (string->number s)))
    (if (exact-integer? n)
	n
	(error "Expected Integer field: ~s" s))))

(: s->r (String -> Float))
(define (s->r s)
  (let ((n (string->number s)))
    (if (real? n)
	(real->double-flonum n)
	(error "Expected Real number field: ~s" s))))

(: read-integer-value (Input-Port -> Integer))
(define (read-integer-value inp)
  (s->i (read-number-string inp)))

(: read-real-value (Input-Port -> Real))
(define (read-real-value inp)
  (s->r (read-number-string inp)))
