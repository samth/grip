#lang typed/racket/base

(require 
 (only-in racket/set
          list->seteq
          set-union)
 (only-in "model.rkt"
          Model mkModel
          variable-cardinalities))

(define-type Assignment (Vectorof Index))

(struct: Factor ([var : (Vectorof Symbol)]
                 [ordinality : (Vectorof Integer)]                  
                 [stride : (Vectorof Index)]
                 [val : FlVector]) #:transparent)

(: mkFactor ((Listof Symbol) (Listof Integer) FlVector -> Factor))
(define (mkFactor vars vcards vals)
  (let ((vallen (flvector-length vals))
        (varlen (apply * vcards)))
    (when (not (eqv? vallen varlen))
      (error "Dimensionality mismatch between variable cardinality and given values cardinality.")))
  (let: loop : Factor
    ((cards : (Listof Integer) vcards)
     (strides : (Listof Index) '())
     (stride : Index 1))
    (if (null? cards)
        (Factor (list->vector vars) 
                (list->vector vcards)
                (list->vector (reverse strides))
                vals)
        (loop (cdr cards)
              (cons stride strides)
              (assert (* stride (car cards)) index?)))))

(: vars-union ((Listof Symbol) (Listof Symbol) -> (Setof Symbol)))
(define (vars-union vars1 vars2)
  (set-union (list->seteq vars1) (list->seteq vars2)))

(: Factor-var-count (Factor -> Index))
(define (Factor-var-count factor)
  (vector-length (Factor-var factor)))

(: assignment-to-index (Factor Assignment -> Index))
(define (assignment-to-index factor assign)  
  (define len (Factor-var-count factor))
  (define strides (Factor-stride factor))  
  (do ([i 0 (add1 i)]
       [index 0 (+ index (* (vector-ref assign i)
                            (vector-ref strides i)))])
    ([>= i len] (assert index index?))))

;; ugly for performance
(: index-to-assignment (Factor Index -> (Vectorof Integer)))
(define (index-to-assignment factor idx)
  (define len (Factor-var-count factor))
  (define stride (Factor-stride factor))
  (do ([j (sub1 len) (sub1 j)]
       [assign (make-vector len 0)]
       [idx idx])
    ([< j 0] assign)    
    (let-values (((q r) (quotient/remainder idx (vector-ref stride j))))
      (vector-set! assign j q)
      (set! idx r))))

;;;;;;;;;;;;;;;;;;;
;; SCRATCH BELOW ;;
;;;;;;;;;;;;;;;;;;;

(require racket/flonum)

(: gen-test-factors ((Listof Symbol) (Listof Integer) -> FlVector))
(define (gen-test-factors model card)
  (let* ((sz (apply * card))
         (v (make-flvector sz 0.0)))
    (do ((idx 0 (add1 idx))
         (val 0.0 (+ val 1.0)))
      ((>= idx sz) v)
      (flvector-set! v idx val))))

(: m-test Model)
(define m-test  
  (mkModel "Test" '(2 3 4)))

(: f-test Factor)
(define f-test
  (let ((vars '(A B C))
        (cards '(2 3 4)))
    (mkFactor vars cards (gen-test-factors vars cards))))

