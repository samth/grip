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

#lang racket

(provide parse-parms encode-parm
	 parms->query)

(require knozama/web/uri/charset
	 (only-in knozama/text/util
		  weave-string-separator)
	 (only-in srfi/14
		  string->char-set
		  char-set-complement)
	 (only-in srfi/13
		  string-tokenize))


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

(define parm-reserved-char?
  (lambda (ch)
    (case ch
      ((#\& #\=) #t)
      (else #f))))

(define alist? list?)
(define query alist?)

(define encode-parm-string
  (lambda (str)
    (let ((op (open-output-string))
	(ip (open-input-string str)))
      (let loop ((ch (read-char ip)))
	(if (eof-object? ch)
	   (get-output-string op)
	   (begin
	     (if (or (unsafe-char? ch)
		   (parm-reserved-char? ch))
		(write-string op (encode-char ch))
		(write-char op ch))
	     (loop (read-char ip))))))))

(define encode-parm
  (lambda (parm)
    (let ((key   (car parm))
	(value (cdr parm)))
      (cons (encode-parm-string key)
	    (encode-parm-string value)))))

(define parms->query
  (lambda (parms)
    (weave-string-separator "&" (map (lambda (kv)
				       (string-append (car kv) "=" (cdr kv)))
				     parms))))

(define parm-delim-char-set
  (char-set-complement (string->char-set "=&")))

(define parse-parms
  (lambda (parm-str)
    (let ((kvs (string-tokenize parm-str parm-delim-char-set)))
      (let loop ((kvs kvs) (parms '()))
	(if (null? kvs)
	   parms
	   (let ((key (car kvs)))
	     (if (null? (cdr kvs))
		parms ;; odd number of KVs which is wrong.  Return what we got.
		(loop (cddr kvs) (cons (cons key (cadr kvs)) parms)))))))))
