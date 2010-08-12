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

(provide assoc-value 
	 ;; assoc-string assoc-string-value
	 lookup
	 last-pair make-list 
	 ;; every? any? 
	 weave)

(require (only-in srfi/1
		  find)
	 (only-in "../std/prelude.rkt"
		  fxzero? fx1-)
	 (only-in "../std/control.rkt"
		  aif))

;;  (primitives assoc-string every? any? list-copy))

;; find an element in a list satisfing the given predicate
;; return the given default value
(define lookup
  (lambda (proc list default)
    (aif (find proc list)
	 it
	 default)))

(define assoc-value
  (lambda( key alst)
    (let ((pair (assoc key alst)))
      (if pair
	 (cdr pair)
	 #f))))

;; (define (assoc-string-value str-key alst)
;;   (let ((pair (assoc-string str-key alst)))
;;     (if pair
;;        (cdr pair)
;;        #f)))

;; Returns the last pair in list.
(define last-pair
  (lambda (x)
    (if (pair? (cdr x))
       (last-pair (cdr x))
       x)))

;; weave an element between a list of elements
(define weave
  (lambda (e lst)
    (if (null? lst)
       lst
       (if (null?  (cdr lst))
	  lst
	  (cons (car lst) (cons e (weave e (cdr lst))))))))

(define make-list
  (lambda (len val)
    (let loop ((len len) (lst '()))
      (if (fxzero? len)
	 lst
	 (loop (fx1- len) (cons val lst))))))

