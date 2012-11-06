#lang typed/racket/base

(provide
 enumeratee-transform
 enumeratee-filter)

(require
 racket/match
 (only-in "iteratee.rkt"  
          Enumeratee
          Iteratee Stream Done Continue))

(: enumeratee-transform (All (O I A) ((O -> I) -> (Enumeratee O I A))))
(define (enumeratee-transform cvt)  
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
                                  (let ((newinner (istep (cvt elem))))
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

(: enumeratee-flatmap (All (O I A) ((O -> (Listof I)) -> (Enumeratee O I A))))
(define (enumeratee-flatmap f-cvt)
  
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


