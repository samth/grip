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
 enumeratee-map
 enumeratee-flatmap
 enumeratee-groupby
 enumeratee-filter)

(require
 racket/match
 (only-in "iteratee.rkt"  
          Enumeratee
          Iteratee Stream Done Continue))

(: enumeratee-map (All (O I A) ((O -> I) -> (Enumeratee O I A))))
(define (enumeratee-map map)  
  (λ: ((inner : (Iteratee I A)))    
    (: step ((Iteratee I A) -> ((Stream O) -> (Iteratee O (Iteratee I A)))))
    (define (step inner)
      (λ: ((elem : (Stream O)))
        (cond
          ((eq? elem 'Nothing) (Continue (step inner)))
          ((eq? elem 'EOS)     (Done 'EOS inner))
          (else                (match inner
                                 [(Done _ _ ) (Done elem inner)]
                                 [(Continue istep)
                                  (let ((newinner (istep (map elem))))
                                    (Continue (step newinner)))])))))    
    (Continue (step inner))))

(: enumeratee-flatmap (All (O I A) (((Listof O) -> I) -> (Enumeratee (Listof O) I A))))
(define (enumeratee-flatmap fmap)  
  (λ: ((inner : (Iteratee I A))) 
    (: step ((Iteratee I A) -> ((Stream (Listof O)) -> (Iteratee (Listof O) (Iteratee I A)))))
    (define (step inner)
      (λ: ((data : (Stream (Listof O))))
        (cond
          ((eq? data 'Nothing) (Continue (step inner)))
          ((eq? data 'EOS)     (Done 'EOS inner))
          (else                (match inner
                                 [(Done _ _ ) (Done data inner)]
                                 [(Continue istep)
                                  (let ((newinner (istep (fmap data))))
                                    (Continue (step newinner)))])))))    
    (Continue (step inner))))

(: enumeratee-filter (All (D A) ((D -> Boolean) -> (Enumeratee D D A))))
(define (enumeratee-filter filter-fn)
  
  (λ: ((inner : (Iteratee D A)))
    
    (: step ((Iteratee D A) -> ((Stream D) -> (Iteratee D (Iteratee D A)))))
    (define (step inner)
      (λ: ((elem : (Stream D)))
        (cond
          [(eq? elem 'Nothing) 
           (Continue (step inner))]
          [(eq? elem 'EOS)
           (Done 'EOS inner)]
          [else (match inner
                  [(Done _ _) 
                   (Done elem inner)]
                  [(Continue istep)
                   (if (filter-fn elem)
                       (Continue (step (istep elem)))
                       (Continue (step inner)))])])))
    
    (Continue (step inner))))

(: enumeratee-groupby (All (D A) ((D D -> Boolean) -> (Enumeratee D (Listof D) A))))
(define (enumeratee-groupby comparer)
  
  (λ: ((inner : (Iteratee (Listof D) A)))
    
    (: step ((Listof D) (Iteratee (Listof D) A) -> ((Stream D) -> (Iteratee D (Iteratee (Listof D) A)))))
    (define (step accum inner)
      (λ: ((datum : (Stream D)))
        (cond 
          ((eq? datum 'Nothing)
           (Continue (step accum inner)))
          ((eq? datum 'EOS)
           (match inner
             [(Done _ _ ) (Done datum inner)]
             [(Continue inner-step)
              (Done 'EOS (inner-step accum))]))
          (else (match inner
                  [(Done _ _) (Done datum inner)]
                  [(Continue inner-step)
                   (if (null? accum)
                       (Continue (step (list datum) inner))
                       (if (comparer datum (car accum))                           
                           (Continue (step '() (inner-step (cons datum accum))))
                           (Continue (step (list datum) (inner-step accum)))))])))))
    
    (Continue (step '() inner))))

(: enumeratee-unfold (All (O I A) ((O -> (Listof I)) -> (Enumeratee O I A))))
(define (enumeratee-unfold f-cvt)
  
  (λ: ((inner : (Iteratee I A)))
    
    (: iter-elems ((Iteratee I A) O -> (Iteratee I A)))
    (define (iter-elems rec-iter elem)
      (let: loop : (Iteratee I A) ((iter : (Iteratee I A) rec-iter)
                                   (elems : (Listof I) (f-cvt elem)))
        (if (null? elems)
            iter
            (match iter
              [(Done _ _) iter]
              [(Continue istep) 
               (loop (istep (car elems)) (cdr elems))]))))
    
    (: step ((Iteratee I A) -> ((Stream O) -> (Iteratee O (Iteratee I A)))))
    (define (step inner)
      (λ: ((elem : (Stream O)))
        (cond 
          ((eq? elem 'Nothing)
           (Continue (step inner)))
          ((eq? elem 'EOS)
           (Done 'EOS inner))
          (else (match inner
                  [(Done _ _ ) (Done elem inner)]
                  [(Continue _)
                   (let ((rec-iter (iter-elems inner elem)))
                     (match rec-iter
                       [(Done _ _) (Done elem rec-iter)]
                       [(Continue _)
                        (Continue (step rec-iter))]))])))))
    
    (Continue (step inner))))


