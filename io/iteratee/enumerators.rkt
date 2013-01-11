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
 enumerator/eos
 enumerator/list
 enumerator/text-input-port
 enumerator/select-from-n-lists)

(require
 racket/match
 (only-in "iteratee.rkt"
          Done Continue Iteratee Enumerator))

(: enumerator/eos (All (D A) (Enumerator D A)))
(define (enumerator/eos iter)
  (match iter
    [(Done _ _) iter]
    [(Continue step) (step 'EOS)]))

;; Useful for testing, i.e. simulate an IO read of a string
(: enumerator/string (All (A) String -> (Enumerator String A)))
(define (enumerator/string str)
  (λ: ((iter : (Iteratee String A)))
    (match iter
      [(Done _ _) iter]
      [(Continue k) (k str)])))

(: enumerator/list (All (D A) (Listof D) -> (Enumerator D A)))
(define (enumerator/list lst)
  (λ: ((iter : (Iteratee D A)))
    (let: loop : (Iteratee D A) ((lst : (Listof D) lst) 
                                 (iter : (Iteratee D A) iter))
      (match (cons lst iter)
        [(cons '() i)           i]
        [(cons _ (Done _ _))    iter]
        [(cons (list-rest x xs) (Continue k)) (loop xs (k x))]))))

(: enumerator/text-input-port (All (A) (Input-Port -> (Enumerator String A))))
(define (enumerator/text-input-port inp)
  (λ: ((iter : (Iteratee String A)))
    (let loop ((iter iter))
      (match iter
        [(Done _ _) iter]
        [(Continue step)
         (let ((line (read-line inp)))
           (if (eof-object? line)
               iter
               (loop (step line))))]))))

;; Given multiple lists select the next element from one of the lists using the selector until all lists are exhausted.
;; selector - when true the first arg is selected else the second
;; e.g, if selector is a less-than? comparison function the enumerator feeds the least head item from the lists.
;;      and if the lists themselves were sorted, the enumerator feed is sorted.
(: enumerator/select-from-n-lists (All (D A) (Listof (Listof D)) (D D -> Boolean) -> (Enumerator D A)))
(define (enumerator/select-from-n-lists data-lsts select-first?)
  
  (define: lsts : (Vectorof (Listof D)) (list->vector data-lsts))
  (define: lsts-cnt : Index (vector-length lsts))
  
  (: head (Natural -> (Option D)))
  (define (head idx)
    (let ((from-lst (vector-ref lsts idx)))
      (if (null? from-lst)
          #f
          (car from-lst))))
  
  (: pop (Natural -> (Option D)))
  (define (pop idx)
    (let ((datum (head idx)))
      (when datum          
        (vector-set! lsts idx (cdr (vector-ref lsts idx))))
      datum))
  
  (: next-selected (-> (Option D)))
  (define (next-selected)
    (let: loop : (Option D) ((idx : Natural 0)
                             (widx : Natural 0)
                             (winner : (Option D) (head 0)))
      (if (>= idx lsts-cnt)          
          (pop widx)
          (let: ((next : (Option D) (head idx)))
            (if next
                (if winner
                    (if (select-first? winner next)
                        (loop (add1 idx) widx winner)
                        (loop (add1 idx) idx next))
                    (loop (add1 idx) idx next))
                (loop (add1 idx) widx winner))))))
  
  (λ: ((iter : (Iteratee D A)))
    (let: loop : (Iteratee D A) ((iter : (Iteratee D A) iter))      
      (match iter
        [(and done (Done _ _)) done]
        [(Continue k)
         (let ((selected (next-selected)))
           (if selected
               (loop (k selected))               
               (k 'EOS)))]))))