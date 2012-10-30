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

(provide 
 parse-params encode-param encode-param-string 
 params->query
 param Param Param? Params)

(require 
 (only-in typed/srfi/14
	  Char-Set
	  string->char-set
	  char-set-complement)
 (only-in "../../../prelude/text/util.rkt"
          weave-string-separator)
 "../uricharset.rkt")

(require/typed 
 srfi/13
 (string-tokenize (String Char-Set -> (Listof String))))

(define-type Param (Pair String String))

(define-type Params (Listof Param))

(define-predicate Param? Param)

(: param (String String -> Param))
(define param cons)

(: param-reserved-char? (Char -> Boolean))
(define (param-reserved-char? ch)
  (case ch
    ((#\& #\=) #t)
    (else #f)))

(: encode-param-string (String Boolean -> String))
(define (encode-param-string str space-as-plus)
  (let ((op (open-output-string))
      (ip (open-input-string str)))
    (let loop ((ch (read-char ip)))
      (cond 
       ((eof-object? ch) (get-output-string op))
       ((char=? ch #\space) 
	(if space-as-plus	   
	   (write-char #\+ op)
	   (write-string "%20" op))
	(loop (read-char ip)))
       ((or (unsafe-char? ch)
	   (param-reserved-char? ch))
	(write-string (encode-char ch) op)
	;;(write-char ch op)
	(loop (read-char ip)))
       (else
	(write-char ch op)
	(loop (read-char ip)))))))

;; (if (eof-object? ch)
;; 	 (get-output-string op)
;; 	 (begin
;; 	   (if (or (unsafe-char? ch)
;; 		 (param-reserved-char? ch)
;; 		 (char=? #\+ ch))
;; 	      (write-string (encode-char ch) op)
;; 	      (write-char ch op))
;; 	   (loop (read-char ip)))))))

(: encode-param (Param Boolean -> Param))
(define (encode-param param space-as-plus)
  (let ((key   (car param))
      (value (cdr param)))
    (cons (encode-param-string key space-as-plus)
	  (encode-param-string value space-as-plus))))

(: params->query (Params -> String))
(define (params->query parms)
  (weave-string-separator "&" (map (lambda:  ((kv : (Pair String String)))
				     (string-append (encode-param-string (car kv) #f)
						    "=" 
						    (encode-param-string (cdr kv) #f)))
				   parms)))

(: param-delim-char-set Char-Set)
(define param-delim-char-set
  (char-set-complement (string->char-set "=&")))

(: parse-params (String -> (Listof Param)))
(define (parse-params param-str)
  (let ((kvs (string-tokenize param-str param-delim-char-set)))
    (let: loop : (Listof (Pair String String)) ((kvs : (Listof String) kvs) (params : (Listof (Pair String String)) '()))
	(if (null? kvs)
	   params
	   (let ((key (car kvs)))
	     (if (null? (cdr kvs))
		params ;; odd number of KVs which is wrong.  Return what we got.
		(loop (cddr kvs) (cons (cons key (cadr kvs)) params))))))))
