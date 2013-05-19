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

;; Name change Enumeratee -> Pump.
;; Well Pump Tank, Enumerator Enumeratee Iteratee

(provide:
 [enumeratee/c-index      (All (O A) (Index -> (EnumerateeC O (Pair O Index) A)))]
 [enumeratee/c-for-each   (All (D A) ((D -> Void) -> (EnumerateeC D D A)))]
 [enumeratee-map          (All (O I A) ((O -> I) -> (Enumeratee O I A)))]
 [enumeratee/c-map        (All (O I A) ((O -> (Option I)) -> (EnumerateeC O I A)))]
 [enumeratee-flatmap      (All (O I A) (((Listof O) -> I) -> (Enumeratee (Listof O) I A)))]
 [enumeratee/c-flatmap    (All (O I A) ((O -> (Option I)) -> (EnumerateeC (Listof O) I A)))]
 [enumeratee-groupby      (All (D A) ((D D -> Boolean) -> (Enumeratee D (Listof D) A)))]
 [enumeratee/c-groupby    (All (D A) ((D D -> Boolean) -> (EnumerateeC D (Listof D) A)))]
 [enumeratee/c-unique     (All (D A) (D -> (EnumerateeC D D A)))]
 [enumeratee-filter       (All (D A) ((D -> Boolean) -> (Enumeratee D D A)))]
 [enumeratee/c-filter     (All (D A) ((D -> Boolean) -> (EnumerateeC D D A)))]
 [enumeratee-filter/map   (All (O I A) ((O -> (Option I)) -> (Enumeratee O I A)))]
 [enumeratee/c-list-count (All (D A) -> (EnumerateeC (Listof D) (Listof (Pair D Natural)) A))]
 [enumeratee/c-filter/map (All (O I A) ((O -> (Option I)) -> (EnumerateeC O I A)))]
 [enumeratee/c-reduce     (All (O I A) (((Listof O) -> (Option I)) -> (EnumerateeC (Listof O) I A)))]
 [enumeratee/c-sort       (All (D A) (D D -> Boolean) -> (EnumerateeC (Listof D) (Listof D) A))])

(require
 racket/match
 (only-in "iteratee.rkt"  
          irun
          Enumeratee EnumerateeC
          Iteratee Stream Done Continue))

(: enumeratee/c-for-each (All (D A) ((D -> Void) -> (EnumerateeC D D A))))
(define (enumeratee/c-for-each for-fn)
  (λ: ((inner : (Iteratee D A)))
      
      (: step ((Iteratee D A) -> ((Stream D) -> (Iteratee D A))))
      (define (step inner)
	(λ: ((elem : (Stream D)))
	    (cond 
	     [(eq? elem 'Nothing)
	      (Continue (step inner))]
	     [(eq? elem 'EOS)
	      (Done 'EOS (irun inner))]
	     [else (match inner
			  [(Done _ _)
			   (Done elem (irun inner))]
			  [(Continue istep)
			   (begin
			     (for-fn elem)
			     (Continue (step (istep elem))))])])))
      
      (Continue (step inner))))

(: enumeratee-filter/map (All (O I A) ((O -> (Option I)) -> (Enumeratee O I A))))
(define (enumeratee-filter/map filter/map)  
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
					  (let ((new-elem (filter/map elem)))
					    (if new-elem
						(let ((newinner (istep new-elem)))
						  (Continue (step newinner)))
						(Continue (step inner))))])))))    
      
      (Continue (step inner))))

(: enumeratee/c-filter/map (All (O I A) ((O -> (Option I)) -> (EnumerateeC O I A))))
(define (enumeratee/c-filter/map filter/map)  
  (λ: ((inner : (Iteratee I A)))    
      
      (: step ((Iteratee I A) -> ((Stream O) -> (Iteratee O A))))
      (define (step inner)
	(λ: ((elem : (Stream O)))
	    (cond
	     ((eq? elem 'Nothing) (Continue (step inner)))
	     ((eq? elem 'EOS)     (Done 'EOS (irun inner)))
	     (else                (match inner
					 [(Done _ _ ) (Done elem (irun inner))]
					 [(Continue istep)
					  (let ((new-elem (filter/map elem)))
					    (if new-elem
						(let ((newinner (istep new-elem)))
						  (Continue (step newinner)))
						(Continue (step inner))))])))))    
      
      (Continue (step inner))))

(: enumeratee/c-map (All (O I A) ((O -> (Option I)) -> (EnumerateeC O I A))))
(define (enumeratee/c-map map-fn)  
  (λ: ((inner : (Iteratee I A)))    
      
      (: step ((Iteratee I A) -> ((Stream O) -> (Iteratee O A))))
      (define (step inner)
	(λ: ((elem : (Stream O)))
	    (cond
	     ((eq? elem 'Nothing) (Continue (step inner)))
	     ((eq? elem 'EOS)     (Done 'EOS (irun inner)))
	     (else                (match inner
					 [(Done _ _ ) 
					  (Done elem (irun inner))]
					 [(Continue istep)
					  (let ((iota (map-fn elem)))
					    (if iota
						(Continue (step (istep iota)))
						(Continue (step inner))))])))))
      
      (Continue (step inner))))

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


(: enumeratee/c-flatmap (All (O I A) ((O -> (Option I)) -> (EnumerateeC (Listof O) I A))))
(define (enumeratee/c-flatmap reducer)  
  
  (λ: ((inner : (Iteratee I A)))
      
      (: step ((Iteratee I A) -> ((Stream (Listof O)) -> (Iteratee (Listof O) A))))
      (define (step inner)
	(λ: ((datum : (Stream (Listof O))))
	    (cond
	     ((eq? datum 'Nothing)
	      (Continue (step inner)))
	     ((eq? datum 'EOS)
	      (Done 'EOS (irun inner)))
	     (else 
	      (let: loop : (Iteratee (Listof O) A)
		    ([data : (Listof O) datum]
		     [inner : (Iteratee I A) inner])
		    (if (null? data)
			(Continue (step inner))
			(match inner
			       [(Done _ _) (Done data (irun inner))]
			       [(Continue istep)
				(let: ((d : (Option I) (reducer (car data))))
				      (if d
					  (loop (cdr data) (istep d))
					  (loop (cdr data) inner)))])))))))
      
      
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

(: enumeratee/c-unique (All (D A) (D -> (EnumerateeC D D A))))
(define (enumeratee/c-unique nada)
  (λ: ((inner : (Iteratee D A)))
      
      (: step ((Iteratee D A) D -> ((Stream D) -> (Iteratee D A))))
      (define (step inner last-datum)
	(λ: ((datum : (Stream D)))
	    (cond 
	     [(eq? datum 'Nothing)
	      (Continue (step inner last-datum))]
	     [(eq? datum 'EOS)
	      (Done 'EOS (irun inner))]
	     [else (match inner
			  [(Done _ _) 
			   (Done datum (irun inner))]
			  [(Continue istep)                   
			   (if (equal? datum last-datum)
			       (Continue (step inner last-datum))
			       (Continue (step (istep datum) datum)))])])))                   
      
      (Continue (step inner nada))))

(: enumeratee/c-filter (All (D A) ((D -> Boolean) -> (EnumerateeC D D A))))
(define (enumeratee/c-filter filter-fn)
  
  (λ: ((inner : (Iteratee D A)))
      
      (: step ((Iteratee D A) -> ((Stream D) -> (Iteratee D A))))
      (define (step inner)
	(λ: ((elem : (Stream D)))
	    (cond
	     [(eq? elem 'Nothing) 
	      (Continue (step inner))]
	     [(eq? elem 'EOS)
	      (Done 'EOS (irun inner))]
	     [else (match inner
			  [(Done _ _) 
			   (Done elem (irun inner))]
			  [(Continue istep)
			   (if (filter-fn elem)
			       (Continue (step (istep elem)))
			       (Continue (step inner)))])])))
      
      (Continue (step inner))))

(: enumeratee/c-groupby (All (D A) ((D D -> Boolean) -> (EnumerateeC D (Listof D) A))))
(define (enumeratee/c-groupby comparer)
  
  (λ: ((inner : (Iteratee (Listof D) A)))
      
      (: step ((Listof D) (Iteratee (Listof D) A) -> ((Stream D) -> (Iteratee D A))))
      (define (step accum inner)
	(λ: ((datum : (Stream D)))
	    (cond 
	     ((eq? datum 'Nothing)
	      (Continue (step accum inner)))
	     ((eq? datum 'EOS)
	      (match inner
		     [(Done _ _ ) (Done 'EOS (irun inner))]
		     [(Continue inner-step)
		      (Done 'EOS (irun (inner-step accum)))]))
	     (else (match inner
			  [(Done _ _) (Done datum (irun inner))]
			  [(Continue inner-step)
			   (if (null? accum)
			       (Continue (step (list datum) inner))
			       (if (comparer datum (car accum))
				   (Continue (step (cons datum accum) inner))
				   (Continue (step (list datum) (inner-step accum)))))])))))
      
      (Continue (step '() inner))))


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
		     [(Done _ _ ) (Done 'EOS inner)]
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


(: enumeratee/c-sort (All (D A) (D D -> Boolean) -> (EnumerateeC (Listof D) (Listof D) A)))
(define (enumeratee/c-sort compare)
  
  (λ: ((inner : (Iteratee (Listof D) A)))
      
      (: step ((Iteratee (Listof D) A) -> ((Stream (Listof D)) -> (Iteratee (Listof D) A))))
      (define (step inner)
	(λ: ((datum : (Stream (Listof D))))
	    (cond
	     ((eq? datum 'Nothing)
	      (Continue (step inner)))     
	     ((eq? datum 'EOS)
	      (Done 'EOS (irun inner)))
	     (else
	      (match inner
		     [(Done _ _) (Done datum (irun inner))]
		     [(Continue inner-step)
		      (if (null? datum)
			  (Continue (step inner))
			  (let: ((datum-sorted : (Listof D) (sort datum compare)))
				(Continue (step (inner-step datum-sorted)))))])))))
      
      (Continue (step inner))))

(define-type (CntL D) (Listof (Pair D Natural)))

(: enumeratee/c-list-count (All (D A) -> (EnumerateeC (Listof D) (CntL D) A)))
(define (enumeratee/c-list-count)
  
  (: list-count (All (D) (Listof D) -> (CntL D)))
  (define (list-count lst)
    (define: ht : (HashTable D Natural) (make-hash))  
    (let: loop : (Listof (Pair D Natural)) 
	  ((lst : (Listof D) lst))
	  (if (null? lst)
	      (hash->list ht)
	      (begin
		(hash-update! ht (car lst) (λ: ((cnt : Natural)) (add1 cnt)) (λ: () 0))
		(loop (cdr lst))))))  

  (λ: ((inner : (Iteratee (Listof (Pair D Natural)) A)))
      
      (: step ((Iteratee (CntL D) A) -> ((Stream (Listof D)) -> (Iteratee (Listof D) A))))
      (define (step inner)
	(λ: ((datum : (Stream (Listof D))))
	    (cond
	     ((eq? datum 'Nothing)
	      (Continue (step inner)))     
	     ((eq? datum 'EOS)
	      (Done 'EOS (irun inner)))
	     (else
	      (match inner
		     [(Done _ _) (Done datum (irun inner))]
		     [(Continue inner-step)              
		      (let: ((datum-counts : (CntL D) (list-count datum)))
			    (Continue (step (inner-step datum-counts))))])))))
      
      (Continue (step inner))))


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

;; A reduce hands over the entire list.
(: enumeratee/c-reduce (All (O I A) (((Listof O) -> (Option I)) -> (EnumerateeC (Listof O) I A))))
(define (enumeratee/c-reduce reduce)
  (λ: ((inner : (Iteratee I A)))
      
      (: step ((Iteratee I A) -> ((Stream (Listof O)) -> (Iteratee (Listof O) A))))
      (define (step inner)
	(λ: ((datum : (Stream (Listof O))))
	    (cond
	     ((eq? datum 'Nothing)
	      (Continue (step inner)))
	     ((eq? datum 'EOS)
	      (Done 'EOS (irun inner)))
	     (else
	      (match inner
		     [(Done _ _) (Done datum (irun inner))]
		     [(Continue inner-step)
		      (let: ((reduced-datum : (Option I) (reduce datum)))
			    (if reduced-datum				
				(Continue (step (inner-step reduced-datum)))
				(Continue (step inner))))])))))
      
      (Continue (step inner))))

(: enumeratee/c-index (All (O A) (Index -> (EnumerateeC O (Pair O Index) A))))
(define (enumeratee/c-index start-at)
  (λ: ((inner : (Iteratee (Pair O Index) A)))
      
      (: step (Index (Iteratee (Pair O Index) A) -> ((Stream O) -> (Iteratee O A))))
      (define (step idx inner)
	(λ: ((datum : (Stream O)))
	    (cond
	     ((eq? datum 'Nothing)
	      (Continue (step idx inner)))
	     ((eq? datum 'EOS)
	      (Done 'EOS (irun inner)))
	     (else
	      (match inner
		     [(Done _ _) (Done datum (irun inner))]
		     [(Continue inner-step)		       
		      (Continue (step (assert (add1 idx) index?)
				      (inner-step (cons datum idx))))])))))
      
      (Continue (step start-at inner))))

;; FIXME RPR - Really need a Enumeratee compose function.
;; (: groupby-aggregate (All (D E A) ((D D -> Boolean) ((Listof D) -> (Option E)) -> (Enumeratee D E A))))
;; (define (reduce is-same? reduce-fn)  
;;   (define: groupbyT : (EnumerateeC D (Listof D) (Listof E))
;;     (enumeratee/c-groupby is-same?))  
;;   (define: mergeT : (EnumerateeC (Listof D) E (Listof E))
;;     (enumeratee/c-reduce reduce-fn))  
;;   (define: sink : (Iteratee E (Listof E))
;;     (list-sink))
;;   (define: list-iteratee : (Iteratee D (Listof E))
;;     (groupbyT (mergeT sink)))  
;;   list-iteratee)
