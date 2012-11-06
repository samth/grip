#lang typed/racket/base

(require
 racket/pretty
 (only-in racket/set 
          set->list)
 (only-in typed/racket
          range)
 (only-in racket/set
          list->set set-empty? set-intersect set-union)
 (only-in racket/vector
          vector-copy)
 (only-in racket/sequence
          sequence->list)
 (only-in "model.rkt"
          Model variable-cardinalities))

(provide:
 [new-factor (Model (Listof Symbol) (Vectorof Float) -> Factor)]
 [f* (Model Factor Factor -> Factor)]
 [factor-partition (Factor -> Float)]
 [factor-normalize (Factor -> Factor)]
 [factor-cardinality (Model Factor -> (Listof Integer))]
 [factor-ordinality (Factor -> (Listof Integer))]
 [factor-ordinality-for-vars (Factor (Listof Symbol) -> (Listof Integer))]
 [factor-to-log-space (Factor -> Factor)]
 [factor-from-log-space (Factor -> Factor)])

(provide
 (struct-out Factor))

(define-type VarOrdMap (HashTable Symbol Integer))
;;(define-type VarId Integer)

(struct: Factor ([vars : (Listof Symbol)]
                 [vals : (Vectorof Float)]))

(: new-factor (Model (Listof Symbol) (Vectorof Float) -> Factor))
(define (new-factor model vars vals)
  (Factor vars vals))

(: for-vector (All (A B) (Vectorof A) (A -> B) -> (Vectorof B)))
(define (for-vector vec cvt)
  (build-vector (vector-length vec)
                (λ: ((idx : Integer))
                  (cvt (vector-ref vec idx)))))

(: f* (Model Factor Factor -> Factor))
(define (f* model f1 f2)
  (let* ((vars1 (list->set (Factor-vars f1)))
         (vars2 (list->set (Factor-vars f2)))
         (common (set-intersect vars1 vars2))
         (all    (set-union vars1 vars2)))
    (let-values (((big-f small-f)
                  (if (> (factor-width model f1)
                         (factor-width model f2))
                      (values f1 f2)
                      (values f2 f1))))
      (let ((sf-idx (index-factor (factor-cardinality model small-f) small-f (set->list common))))
        (when (set-empty? common)
          (error "Dimensionality mismatch in factors"))
        f1))))

(: factor-ordinality (Factor -> (Listof Integer)))
(define (factor-ordinality factor)
  (range 0 (length (Factor-vars factor)) 1))

(: factor-partition (Factor -> Float))
(define (factor-partition factor)
  (let* ((vals (Factor-vals factor))
         (len (vector-length vals)))
    (do ((idx 0 (add1 idx))
         (sum 0.0 (+ sum (vector-ref vals idx))))
      ((= idx len) sum))))

(: factor-normalize (Factor -> Factor))
(define (factor-normalize factor)
  (let* ((vals (Factor-vals factor))        
         (len (vector-length vals))
         (new-vals (vector-copy vals))
         (partition (factor-partition factor)))
    (do ((idx 0 (add1 idx)))
      ((= idx len) (Factor (Factor-vars factor) new-vals))
      (vector-set! new-vals idx (/ (vector-ref vals idx) partition)))))

(: factor-ordinality-for-vars (Factor (Listof Symbol) -> (Listof Integer)))
(define (factor-ordinality-for-vars factor vars)
  (: ordmap (HashTable Symbol Integer))
  (define ordmap (make-hash))
  (do ((fvars (Factor-vars factor) (cdr fvars))
       (idx 0 (add1 idx)))
    ((null? fvars) ordmap)
    (hash-set! ordmap (car fvars) idx))
  (map (λ: ((var : Symbol)) (hash-ref ordmap var)) vars))

(: factor-cardinality (Model Factor -> (Listof Integer)))
(define (factor-cardinality model factor)
  (variable-cardinalities model (Factor-vars factor)))

(: factor-width (Model Factor -> Integer))
(define (factor-width model factor)
  (apply + (factor-cardinality model factor)))

(: factor-to-log-space (Factor -> Factor))
(define (factor-to-log-space factor)
  (: real-log (Float -> Float))
  (define (real-log v)
    (assert (log v) flonum?))
  (struct-copy Factor factor 
               [vals (for-vector (Factor-vals factor) real-log)]))

(: factor-from-log-space (Factor -> Factor))
(define (factor-from-log-space factor)
  (: real-exp (Float -> Float))
  (define (real-exp v)
    (exp v))
  (struct-copy Factor factor 
               [vals (for-vector (Factor-vals factor) real-exp)]))


(define-type FEntry (Pair (Vectorof Integer) Float))
(define-type FIndex (HashTable (Vectorof Integer)(Listof FEntry)))

(struct: FactorIndex ([vars : (Listof Symbol)]
                      [ords : (Listof Integer)]
                      [index : FIndex]))

;; Index a factor on the given vars.
;; Given a Factor in A,B,C and index vars of (C, B)
;; Then the map contains an index entry of ((c0 b0) . '((a0 b0 c0) (a1 b0 c0)))
(: index-factor ((Listof Integer) Factor (Listof Symbol) -> FactorIndex))
(define (index-factor cards factor vars)
  
  (define N (length vars))
  
  (: index FIndex)
  (define index (make-hash))
  
  (: vord (Listof Integer))
  (define vord (factor-ordinality-for-vars factor vars))
  
  (: indexer (Integer (Vectorof Integer) -> Void))
  (define (indexer val-idx param)
    (define key (make-vector N))
    (do ((idx 0 (add1 idx))
         (vord vord (cdr vord)))
      ((= idx N) (void))
      (vector-set! key idx (vector-ref param (car vord))))
    (hash-update! index key
                  (λ: ((params : (Listof FEntry))) 
                    (cons (cons param (vector-ref (Factor-vals factor) val-idx)) params))
                  (λ () '())))
  
  ;; idx The index into the Factor val vector.
  ;; parameter The parameter ax bx cx ...
  (: index-parameter (Integer (Vectorof Integer) -> Void))
  (define (index-parameter val-idx parameter)
    (display vord)(display " - ")(display val-idx) (display " - ")
    (displayln parameter)
    (indexer val-idx parameter)
    (void))
  
  (for/parameters cards index-parameter)
  (pretty-print index)
  (FactorIndex vars vord index))


(: index-to-assignment (Integer (Listof Integer) -> (Listof Integer)))
(define (index-to-assignment idx card)
  
  (: cum-prod ((Listof Integer) -> (Listof Integer)))
  (define (cum-prod cards)
    (if (pair? cards)
        (let ((init-accum (list (car cards))))
          (let: loop : (Listof Integer) ((cards : (Listof Integer)(cdr cards)) (accum : (Listof Integer)init-accum))
            (if (null? cards)
                (cdr accum)
                (loop (cdr cards) (cons (* (car accum) (car cards)) 
                                        accum)))))
        '()))
  
  (let ((radixs (cum-prod card)))
    (let loop ((idx idx) 
               (radixs radixs) 
               (assign (ann '() (Listof Integer))))
      (if (null? radixs)
          (cons idx assign)
          (let-values (((q r) (quotient/remainder idx (car radixs))))
            (loop r (cdr radixs) (cons q assign)))))))

;; Generate all the parameters.
;; Create a "register" of the size of the number of variables and increment modulo the card of each variable.
;; As each parameter is generated invoke the given function with the current parameter and the current index value.
(: for/parameters ((Listof Integer) (Integer (Vectorof Integer) -> Void) -> Void))
(define (for/parameters cards fn)
  (define N (sub1 (apply * cards)))
  (define vcard (list->vector cards))
  (define reg (make-vector (length cards)))
  (: inc-and-carry (Integer -> Void))
  (define (inc-and-carry idx)
    (let ((n (add1 (vector-ref reg idx))))
      (if (< n (vector-ref vcard idx))
          (vector-set! reg idx n)
          (begin
            (vector-set! reg idx 0)
            (inc-and-carry (add1 idx))))))
  (do ((i 0 (add1 i)))
    ((>= i N) (fn i reg))    
    (fn i (vector-copy reg))
    (inc-and-carry 0)))
