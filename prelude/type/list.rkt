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

(provide assoc-value 
	 ;; assoc-string assoc-string-value
	 lookup
	 last-pair make-list
	 ;; every? any? 
	 weave)

(provide: 
 [zip (All (A B) (Listof A) (Listof B) -> (Listof (Pair A B)))])

(require/typed 
 srfi/1
 (find (All (a) (a -> Boolean) (Listof a) -> (Option a))))

(require 
 (only-in "../std/control.rkt"
	  aif))

;;  (primitives assoc-string every? any? list-copy))

;; find an element in a list satisfing the given predicate
;; return the given default value
(: lookup (All (a) (a -> Boolean) (Listof a) a -> a))
(define (lookup proc list default)
  (let ((e (find proc list)))
    (if e
       e
       default)))

(: assoc-value (All (a b) a (Listof (Pairof a b)) -> (Option b)))
(define (assoc-value key alst)
  (let ((pair (assoc key alst)))
    (if pair
       (cdr pair)
       #f)))

;; (define (assoc-string-value str-key alst)
;;   (let ((pair (assoc-string str-key alst)))
;;     (if pair
;;        (cdr pair)
;;        #f)))

;; Returns the last pair in list.
(: last-pair (All (a) (Listof a) -> (Listof a)))
(define (last-pair x)
  (if (pair? (cdr x))
     (last-pair (cdr x))
     x))

;; weave an element between a list of elements
(: weave (All (a) a (Listof a) -> (Listof a)))
(define (weave e lst)
  (if (or (null? lst)
	  (null? (cdr lst)))
      lst
      (cons (car lst) (cons e (weave e (cdr lst))))))
  
(: make-list (All (a) Integer a -> (Listof a)))
(define (make-list len val)
  (let: loop : (Listof a) ((len : Integer len) (lst  : (Listof a) '()))
    (if (zero? len)
       lst
       (loop (add1 len) (cons val lst)))))

(: zip (All (A B) ((Listof A) (Listof B) -> (Listof (Pair A B)))))
(define (zip lsta lstb)
  (let: loop : (Listof (Pair A B)) ((as : (Listof A) lsta) 
                                    (bs : (Listof B) lstb) 
                                    (accum : (Listof (Pair A B))'()))
    (if (or (null? as)
            (null? bs))
        (reverse accum)
        (loop (cdr as) (cdr bs) (cons (cons (car as) (car bs)) accum)))))

