#lang typed/racket/base

(struct: (D C) Collection ([append   : (D -> Void)]
                           [build    : (-> C)]))

(: List-Builder (All (D) -> (Collection D (Listof D))))
(define (List-Builder)
  
  (: lst (Listof D))
  (define lst '())
  
  (Collection (λ: ((datum : D))
                (set! lst (cons datum lst)))
              (λ () lst)))
                
(: build-Collection (All (D C) (Collection D C)-> C))
(define (build-Collection collection)
  ((Collection-build collection)))

(: append-Collection (All (D C) (Collection D C) D -> Void))
(define (append-Collection coll elem)
  (((inst Collection-append D C) coll) elem)) ;; bug with having to create inst ...

;;;;;;;;;;;;

(require 
 racket/match)

(define-type Existential (All (D) (U 'Nothing D)))

(: tis (All (D) (Existential D) -> (Listof D)))
(define (tis thing)
  (cond 
    [(eq? thing 'Nothing) '()]
    [else (list thing)]))


