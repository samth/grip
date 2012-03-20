;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Knozama's Amazon API Library
;; Copyright (C) 2007-2012  Raymond Paul Racine
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
 (struct-out Failure)
 (struct-out Success)
 failed? succeded? get invert
 map/try map/try-or-else flatmap/try foreach/try filter/try exists
 success-or-else-try
 rescue recover log-on-failure
 try continue with-try with-try-or-else)

(require
 racket/match)

(struct: (T) Failure ([exception : exn]))

(struct: (T) Success ([result : T]))

(define-type (Try T) (U (Failure T) (Success T)))

(: failed? (All (T) (Try T) -> Boolean))
(define (failed? try)
  (Failure? try))

(: succeded? (All (T) (Try T) -> Boolean))
(define (succeded? try)
  (Success? try))

(: get (All (T) (Try T) -> T))
(define (get try)
  (match try
   ((Failure exn) (raise exn))
   ((Success result) result)))
 
(: success-or-else-try (All (T) (Try T) (-> T) -> (Try T)))
(define (success-or-else-try try or-else)
  (match try
   ((Success _) try)
   ((Failure exn)
    (with-try (or-else)))))

;; Is a generic "lens" library on structures possible?? 
;; Higher kinded types, L is a type constructor binding for M (Success) and N (Failure)
;;  (: S-map (All (L T U) (L T) (T -> U) -> (L U)))
(: map/try (All (T U) (Try T) (T -> U) -> (Try U)))
(define (map/try try fn)
  (match try
    ((Success result)
     (with-try (fn result)))
    ((Failure exn) 
     (Failure exn)))) ;; should be able to return the 'try'

(: flatmap/try (All (T U) (Try T) (T -> (Try U)) -> (Try U)))
(define (flatmap/try try fn)
  (match try
    ((Success result)
     (with-handlers ([exn:fail? (λ: ((ex : exn))
				    (Failure ex))])
       (fn result)))
    ((Failure exn) (Failure exn))))

(: map/try-or-else (All (T U) (Try T) (T -> U) (-> U) -> (Try U)))
(define (map/try-or-else try fn fail)
  (match try
   ((Success result)
    (with-try
     (fn result)))
   ((Failure exn)
    (with-try (fail)))))

(: foreach/try (All (T U) (Try T) (T -> U) -> Void))
(define (foreach/try try fn)
  (match try
    ((Success result)
     (fn result)
     (void))
    ((Failure _)
     (void))))

(: filter/try (All (T) (Try T) (T -> Boolean) -> (Try T)))
(define (filter/try try select?)
  (match try
   ((Success result) 
    (if (select? result)
	try
	(Failure (exn:fail (format "Unsatisfied try filter predicate for ~s" try)
			   (current-continuation-marks)))))
   ((Failure exn) (Failure exn))))
	
(: exists (All (T) (Try T) (T -> Boolean) -> Boolean))
(define (exists try exists?)
  (match try
    ((Success result)
     (exists? result))
    ((Failure _) #f)))

(: invert ((Try exn) -> (Try exn)))
(define (invert try)
  (match try
   ((Success result)
    (Failure (exn:fail (format "Inverted try. Success for ~s is Failure" result)
		       (current-continuation-marks))))
   ((Failure exn)
    (Success exn))))

(: continue (All (T V W) (Try T) (T -> (Try V)) (exn -> (Try W)) -> (Try (U V W))))
(define (continue try on-success on-failure)
  (match try
   ((Success result)
    (on-success result))
   ((Failure exn)
    (on-failure exn))))

(: log-on-failure (All (T) (Try T) (exn -> Void) -> (Try T)))
(define (log-on-failure try logger)
  (continue
   try
   (λ x try)
   (λ: ((exn : exn)) 
       (logger exn)
       try)))

;; Rescue a failed computation if the rescue function is capable of doing so.
;; Careful of side effects by the rescue function.
;; Should my-hero be a PartialFunction as opposed to one which returns Option.
(: rescue (All (T) (Try T) ((Failure T) -> (Option (Try T))) -> (Try T)))
(define (rescue try my-hero)
  (with-handlers ([exn:fail? (λ (ex)
  				(Failure ex))])
    (cond
      ((Failure? try)
       (let ((result (my-hero try)))
	 (if result
	     result
	     try)))
     ((Success? try) try))))

(: recover (All (T) (Try T) ((Failure T) -> (Option T)) -> (Try T)))
(define (recover try my-heroine)
  (with-handlers ([exn:fail? (λ (ex)
				(Failure ex))])
    (cond
     ((Failure? try)
      (let ((result (my-heroine try)))
	(if result
	    (Success result)
	    try)))
     ((Success? try) try))))

(: try (All (T) (-> T) -> (Try T)))
(define (try thunk)
  (with-handlers ([exn:fail? (λ (ex)
				(Failure ex))])
    (Success (thunk))))

(define-syntax with-try
  (syntax-rules ()
    ((_ body ...)
     (try (λ () body ...)))))

(define-syntax with-try-or-else
  (syntax-rules ()
    ((_ attempt or-else)
     (success-or-else (try (λ () attempt))
		      (λ (try) or-else)))))

