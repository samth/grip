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
 iseq icomplete 
 Iteratee Stream
 (struct-out Continue)
 (struct-out Done)
 Enumerator eseq
 Enumeratee)

(require racket/match)

(define-type (Stream D) (U D 'Nothing 'EOS))

(define-type (Iteratee D A) (U (Done D A) (Continue D A)))

(struct: (D A) Done ([stream : (Stream D)]
                     [accum : A]))

(struct: (D A) Continue ([step : ((Stream D) -> (Iteratee D A))]))	 

(: icomplete (All (D A) (Iteratee D A) -> A))
(define (icomplete iter)
  (match iter
    [(Done _ accum)  accum]
    [(Continue step) (icomplete (step 'EOS))]))

(: iseq (All (D A B) ((Iteratee D A) (A -> (Iteratee D B)) -> (Iteratee D B))))
(define (iseq iter fn)
  (match iter
    [(Done d a) (fn a)]
    [(Continue step) (Continue (λ: ((d : (Stream D))) 
                                 (iseq (step d) fn)))]))

(define-type (Enumerator D A) ((Iteratee D A) -> (Iteratee D A)))

(: eseq (All (D A) (Enumerator D A) (Enumerator D A) -> (Enumerator D A)))
(define (eseq e1 e2)
  (λ (iter) (e2 (e1 iter))))

(define-type (Enumeratee O I A) ((Iteratee I A) -> (Iteratee O (Iteratee I A))))
