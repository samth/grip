;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Ray Racine's TR Library
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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#lang typed/racket/base

(provide
 lister
 counter drop 
 head head-n
 sum 
 print
 rev upcase
 and-iteratee)

(require  
 racket/match
 (only-in "iteratee.rkt"
	  Iteratee Stream Continue Done))

(: counter (All (D) (-> (Iteratee D Integer))))
(define (counter)
  (: step (Integer -> ((Stream D) -> (Iteratee D Integer))))
  (define (step n)
    (λ: ((str : (Stream D)))
	(match str
	  ['Nothing  (Continue (step n))]
	  ['EOS      (Done 'EOS n)]
	  [_         (Continue (step (add1 n)))])))
  (Continue (step 0)))

(: drop (All (D) Integer -> (Iteratee D Void)))
(define (drop n)
  (: step (-> ((Stream D) -> (Iteratee D Void))))
  (define (step) 
    (λ: ((str : (Stream D)))
	(match str
	  ['Nothing  (Continue (step))]
	  ['EOS      (Done 'EOS (void))]
	  [_         (drop (sub1 n))])))
  (if (zero? n)
      (Done 'Nothing (void))
      (Continue (step))))

(: head (All (D) (-> (Iteratee D (Option D)))))
(define (head)
  (: step ((Stream D) -> (Iteratee D (Option D))))
  (define step
    (λ (str)
       (cond
        [(eq? str 'Nothing)  (Continue step)]
        [(eq? str 'EOS)      (Done 'EOS #f)]
        [else                 (Done 'EOS str)])))
  (Continue step))

(: head-n (All (D) (Integer -> (Iteratee D (Listof D)))))
(define (head-n n)
  
  (: head-n-accum (Integer (Listof D) -> (Iteratee D (Listof D))))
  (define (head-n-accum n accum)
    
    (: step (Integer (Listof D) -> ((Stream D) -> (Iteratee D (Listof D)))))
    (define (step n accum)
      (λ: ((s : (Stream D)))
	  (cond
	   [(eq? s 'Nothing) (Continue  (step n accum))]
	   [(eq? s 'EOS)     (Done 'EOS (reverse accum))]
	   [else             (head-n-accum (sub1 n) (cons s accum))])))
    
    (if (zero? n)
        (Done 'Nothing (reverse accum))
        (Continue (step n accum))))
  
  (head-n-accum n '()))

(: lister (All (D) (-> (Iteratee D (Listof D)))))
(define (lister)
  
  (: step ((Listof D) -> ((Stream D) -> (Iteratee D (Listof D)))))
  (define (step lst)
    (λ: ((s : (Stream D)))
	(cond
	 [(eq? s 'Nothing)
	  (Continue (step lst))]
	 [(eq? s 'EOS)
	  (Done 'EOS (reverse lst))]
	 [else (Continue (step (cons s lst)))])))
  
  (Continue (step '())))

;; Predicate Iteratees

;; All datum elements satisfy the given predicate
(: and-iteratee (All (D) ((D -> Boolean) -> (Iteratee D Boolean))))
(define (and-iteratee pred)
  
  (: step ((Stream D) -> (Iteratee D Boolean)))
  (define (step input)
    (cond
     [(eq? input 'Nothing)  (Continue step)]
     [(eq? input 'EOS)      (Done 'EOS #t)]
     [else (if (pred input) (Continue step) (Done input #f))]))
  
  (Continue step))

(: sum (-> (Iteratee Integer Integer)))
(define (sum)
  (: step (Integer -> ((Stream Integer) -> (Iteratee Integer Integer))))
  (define (step total)
    (λ: ((str : (Stream Integer)))
	(cond
	 ([number? str] (Continue (step (+ str total))))
	 [(eq? str 'Nothing) (Continue (step total))]
	 [(eq? str 'EOS)     (Done 'EOS total)])))
  (Continue (step 0)))

(: print (All (D) (Output-Port -> (Iteratee D Void))))
(define (print outp)
  (: step ((Stream D) -> (Iteratee D Void)))
  (define step
    (λ: ((str : (Stream D)))
	(match str
	  ('Nothing (Continue step))
	  ('EOS     (Done 'EOS (void)))
	  (s        (begin
		      (write s outp)
		      (Continue step))))))
  (Continue step))

;; silly example
(: upcase (-> (Iteratee String String)))
(define (upcase)
  (: step (String -> ((Stream String) -> (Iteratee String String))))
  (define (step str)   
    (λ: ((elem : (Stream String)))
	(cond
	 ((eq? elem 'Nothing) 
	  (Continue (step str)))
	 ((eq? elem 'EOS)     
	  (Done 'EOS (string-upcase str)))
	 (else (Continue (step (string-append " " elem str)))))))
  (Continue (step "")))

;; silly example
(: rev (String -> (Iteratee String String)))
(define (rev str)
  (: step (String -> ((Stream String) -> (Iteratee String String))))
  (define (step str)
    (λ: ((elem : (Stream String)))
	(cond
	 ((eq? elem 'Nothing) 
	  (Continue (step str)))
	 ((eq? elem 'EOS)
	  (Done 'EOS (list->string (reverse (string->list str)))))
	 (else       (Continue (step (string-append elem str)))))))
  (Continue (step str)))
