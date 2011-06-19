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

(provide parse-parms encode-parm
	 parms->query)

(require 
 (only-in typed/srfi/14
	  Char-Set
	  string->char-set
	  char-set-complement)
 (only-in (planet knozama/common:1/text/util)
	  weave-string-separator)
 "../uricharset.rkt")


(require/typed 
 srfi/13
 (string-tokenize (String Char-Set -> (Listof String))))

;; (require (rnrs base)
;; 	 (only (rnrs io simple)
;; 	       eof-object?)
;; 	 (only (rnrs io ports)
;; 	       open-string-input-port
;; 	       get-char put-char put-string)
;; 	 (only (rl3 web uri char-sets)
;; 	       encode-char unsafe-char?)
;; 	 (only (rl3 types chars)
;; 	       string->char-set
;; 	       char-set-complement)
;; 	 (only (rl3 types strings)
;; 	       string-tokenize)
;; 	 (only (rl3 text text)
;; 	       weave-string-separator)
;; 	 (primitives get-output-string open-output-string))

(: parm-reserved-char? (Char -> Boolean))
(define (parm-reserved-char? ch)
  (case ch
    ((#\& #\=) #t)
    (else #f)))


(: encode-parm-string (String -> String))
(define (encode-parm-string str)
  (let ((op (open-output-string))
      (ip (open-input-string str)))
    (let loop ((ch (read-char ip)))
      (if (eof-object? ch)
	 (get-output-string op)
	 (begin
	   (if (or (unsafe-char? ch)
		 (parm-reserved-char? ch))
	      (write-string (encode-char ch) op)
	      (write-char ch op))
	   (loop (read-char ip)))))))

(: encode-parm ((Pair String String) -> (Pair String String)))
(define (encode-parm parm)
  (let ((key   (car parm))
      (value (cdr parm)))
    (cons (encode-parm-string key)
	  (encode-parm-string value))))

(: parms->query ((Listof (Pair String String)) -> String))
(define (parms->query parms)
  (weave-string-separator "&" (map (lambda:  ((kv : (Pair String String)))
				     (string-append (car kv) "=" (cdr kv)))
				   parms)))

(: parm-delim-char-set Char-Set)
(define parm-delim-char-set
  (char-set-complement (string->char-set "=&")))

(: parse-parms (String -> (Listof (Pairof String String))))
(define (parse-parms parm-str)
  (let ((kvs (string-tokenize parm-str parm-delim-char-set)))
    (let: loop : (Listof (Pair String String)) ((kvs : (Listof String) kvs) (parms : (Listof (Pair String String)) '()))
	(if (null? kvs)
	   parms
	   (let ((key (car kvs)))
	     (if (null? (cdr kvs))
		parms ;; odd number of KVs which is wrong.  Return what we got.
		(loop (cddr kvs) (cons (cons key (cadr kvs)) parms))))))))
