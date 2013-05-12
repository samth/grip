#lang typed/racket/base

(provide 
 Lens)

(provide:
 [make-lens (All (A B) (A -> B) (B A -> A) -> (Lens A B))]
 [and-then (All (A B C) (Lens A B) (Lens B C) -> (Lens A C))]
 [compose (All (A B C) (Lens B C) (Lens A B) -> (Lens A C))]
 [trivial (All (A) (Lens A Void))]
 [self (All (A) (Lens A A))])

(define-type Lens (All (A B) (case->  (A -> B) (B A -> A))))

(: make-lens (All (A B) (A -> B) (B A -> A) -> (Lens A B)))
(define (make-lens get set)
  (case-lambda
    [(a) (get a)]
    [(b a) (set b a)]))

(: get (All (A B) (Lens A B) -> (A -> B)))
(define (get lens-ab)
  (λ: ((a : A)) (lens-ab a)))

(: set (All (A B) (Lens A B) -> (B A -> A)))
(define (set lens-ab)
  (λ: ((b : B) (a : A))
    (lens-ab b a)))

(: mod (All (A B) (Lens A B) (B -> B) A -> A))
(define (mod lens f a)
  (lens (f (lens a)) a))

(: and-then (All (A B C) (Lens A B) (Lens B C) -> (Lens A C)))
(define (and-then lens-ab lens-bc)
  (case-lambda
    [(a) (lens-bc (lens-ab a))]
    [(c a) (mod lens-ab (λ: ((b : B)) (lens-bc c b)) a)]))
  
(: compose (All (A B C) (Lens B C) (Lens A B) -> (Lens A C)))
(define (compose lens-ab lens-ca)
  (and-then lens-ca lens-ab))

;;;;;;;;;;;;;;;;;;;;;
;; Some basic Lens ;;
;;;;;;;;;;;;;;;;;;;;;

;; Extract an A from Void
;; Insert an A into a Void
(: trivial (All (A) (Lens A Void)))
(define trivial 
  (case-lambda
    [(a) (void)]
    [(_ a) a]))

;; Insert and Extract an A into an A
;; To meet the Lens laws the "first" A must be returned.
(: self (All (A) (Lens A A)))
(define self 
  (case-lambda
    [(a) a]
    [(a _) a]))

;; Play with structures

(struct: Address ([line : String] 
                  [zip : Integer]) #:transparent)

(struct: Name ([first : String] 
               [last : String]) #:transparent)

(struct: Person ([name : Name]
                 [address : Address]) #:transparent)

;(: name-lens (Lens Person Name))
;(define name-lens
;  (case-lambda 
;    [(person) (Person-name person)]
;    [(person new-name) (struct-copy Person person [name new-name])]))
  
