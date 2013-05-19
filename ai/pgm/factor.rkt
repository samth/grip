#lang typed/racket/base

(require
 racket/pretty
 racket/fixnum
 racket/flonum
 (only-in racket/set 
          set->list)
 (only-in typed/racket
          range)
 (only-in racket/set
          list->set set-empty? set-intersect set-union)
 (only-in racket/vector
          vector-copy)
 (only-in racket/sequence
          sequence->list))

(define-predicate IndexList? (Listof Index))

(provide:
 [mkFactor ((Listof Symbol) (Listof Index) FlVector -> Factor)]
 [f* (Factor Factor -> Factor)]
 [factor-partition (Factor -> Float)]
 [factor-normalize (Factor -> Factor)]
 [factor-cardinality (Factor -> (Vectorof Index))]
 [factor-ordinality (Factor -> (Listof Index))]
 [factor-ordinality-for-vars (Factor (Listof Symbol) -> (Listof Index))]
 [factor-to-log-space (Factor -> Factor)]
 [factor-from-log-space (Factor -> Factor)])

(provide
 (struct-out FModel)
 (struct-out Factor))

(define-type Assignment (Vectorof Index))

(define-type VarOrdMap (HashTable Symbol Index))

(struct: FModel ([vars : (Listof Symbol)]
		 [card : (Vectorof Index)]
		 [stride : (Vectorof Index)]) #:transparent)

(struct: Factor ([fmodel : FModel]
                 [vals : FlVector]) #:transparent)

(: factor-vars (Factor -> (Listof Symbol)))
(define (factor-vars f)
  (FModel-vars (Factor-fmodel f)))

(: factor-cardinality (Factor -> (Vectorof Index)))
(define (factor-cardinality f)
  (FModel-card (Factor-fmodel f)))

(: mkFactor ((Listof Symbol) (Listof Index) FlVector -> Factor))
(define (mkFactor vars vcards vals)
  (let ((vallen (flvector-length vals))
        (varlen (apply * vcards)))
    (when (not (eqv? vallen varlen))
      (error "Dimensionality mismatch between variable cardinality and given values cardinality.")))
  (let: loop : Factor
	((cards : (Listof Index) vcards)
	 (strides : (Listof Fixnum) '())
	 (stride : Fixnum 1))
	(if (null? cards)
	    (Factor (FModel vars
                            (list->vector (assert vcards IndexList?))
                            (list->vector (assert (reverse strides) IndexList?)))
		    vals)
	    (loop (cdr cards)
		  (cons stride strides)
		  (fx* stride (car cards))))))

(: map-flvector (FlVector  (Flonum -> Flonum) -> FlVector))
(define (map-flvector vec cvt)
  (define l (flvector-length vec))
  (let ((v (make-flvector l)))
    (do: : FlVector ((i : Nonnegative-Fixnum 0 (fx+ i 1)))
	 ((>= i l) v)
	 (flvector-set! v i (flvector-ref vec i)))))

(: f* (Factor Factor -> Factor))
(define (f* f1 f2)
  (let* ((vars1 (list->set (factor-vars f1)))
	 (vars2 (list->set (factor-vars f2)))
	 (common (set-intersect vars1 vars2))
	 (all    (set-union vars1 vars2)))
    (pretty-print common)
    (pretty-print all)
    (let-values (((big-f small-f)
		  (if (> (factor-width f1)
			 (factor-width f2))
		      (values f1 f2)
		      (values f2 f1))))
      (pretty-print big-f)
      (pretty-print small-f)
      (let ((sf-idx (index-factor small-f (set->list common))))
	(pretty-print sf-idx)
	(when (set-empty? common)
	  (error "Dimensionality mismatch in factors"))
	f1))))

(: factor-ordinality (Factor -> (Listof Index)))
(define (factor-ordinality factor)
  (range 0 (length (factor-vars factor)) 1))

(: factor-partition (Factor -> Float))
(define (factor-partition factor)
  (let* ((vals (Factor-vals factor))
         (len  (flvector-length vals)))
    (do ((idx 0 (add1 idx))
         (sum 0.0 (+ sum (flvector-ref vals idx))))
	((= idx len) sum))))

(: factor-normalize (Factor -> Factor))
(define (factor-normalize factor)
  (let* ((vals (Factor-vals factor))        
         (len (flvector-length vals))
         (new-vals (flvector-copy vals))
         (partition (factor-partition factor)))
    (do ((idx 0 (add1 idx)))
	((= idx len) (Factor (Factor-fmodel factor) new-vals))
      (flvector-set! new-vals idx (/ (flvector-ref vals idx) partition)))))

(: factor-ordinality-for-vars (Factor (Listof Symbol) -> (Listof Index)))
(define (factor-ordinality-for-vars factor vars)
  (: ordmap (HashTable Symbol Index))
  (define ordmap (make-hash))
  (do: : VarOrdMap ((fvars : (Listof Symbol) (factor-vars factor) (cdr fvars))
		    (idx : Fixnum 0 (fx+ idx 1)))
       ((null? fvars) ordmap)
       (hash-set! ordmap (car fvars) (assert idx index?)))
  (map (λ: ((var : Symbol)) (hash-ref ordmap var)) vars))

(: factor-size (Factor -> Fixnum))
(define (factor-size factor)
  (cardinality-size (factor-cardinality factor)))

(: cardinality-size ((Vectorof Index) -> Fixnum))
(define (cardinality-size cards)
  (let: ((n : Fixnum (vector-length cards)))
    (do: : Fixnum ((i : Fixnum 0 (fx+ i 1))
		   (prod : Fixnum 1 (fx* prod (vector-ref cards i))))
	 ((fx>= i n) prod))))

(: factor-width (Factor -> Index))
(define (factor-width factor)
  (let ((card (factor-cardinality factor)))
    (let ((n (vector-length card)))
      (let loop ((idx 0) (sum 0))
	(if (< idx n)
	    (loop (fx+ 1 idx) (fx+ sum (vector-ref card idx)))
	    (assert sum index?))))))

(: factor-to-log-space (Factor -> Factor))
(define (factor-to-log-space factor)
  (: real-log (Float -> Float))
  (define (real-log v)
    (assert (log v) flonum?))
  (struct-copy Factor factor 
               [vals (map-flvector (Factor-vals factor) real-log)]))

(: factor-from-log-space (Factor -> Factor))
(define (factor-from-log-space factor)
  (: real-exp (Float -> Float))
  (define (real-exp v)
    (exp v))
  (struct-copy Factor factor 
               [vals (map-flvector (Factor-vals factor) real-exp)]))

(define-type FEntry (Pair (Vectorof Index) Float))

(define-type FIndex (HashTable (Vectorof Index) (Listof FEntry)))

(struct: FactorIndex ([vars : (Listof Symbol)]
                      [ords : (Listof Index)]
		      [index : FIndex]) #:transparent)

;; Index a factor on the given vars.
;; Given a Factor in A,B,C and index vars of (C, B)
;; Then the map contains an index entry of ((c0 b0) . '((a0 b0 c0) (a1 b0 c0)))
(: index-factor (Factor (Listof Symbol) -> FactorIndex))
(define (index-factor factor vars)
  
  (define N (length vars))
  
  (define cards (factor-cardinality factor))

  (: index FIndex)
  (define index (make-hash))
  
  (: vord (Listof Index))
  (define vord (factor-ordinality-for-vars factor vars))
  
  (: indexer (Index Assignment -> Void))
  (define (indexer val-idx param)
    (define: key : (Vectorof Index) (make-vector N #{0 : Index}))
    (do: : Void ((idx : Fixnum 0 (fx+ idx 1))
		 (vord vord (cdr vord)))
	 ((= idx N) (void))
	 (vector-set! key idx (vector-ref param (car vord)))
	 (hash-update! index key
		       (λ: ((params : (Listof FEntry))) 
			   (cons (cons param (flvector-ref (Factor-vals factor) val-idx)) params))
		       (λ () '()))))
  
  ;; idx The index into the Factor val vector.
  ;; parameter The parameter ax bx cx ...
  (: index-parameter (Index Assignment -> Void))
  (define (index-parameter val-idx parameter)
    (indexer val-idx parameter)
    (void))
  
  (map-parameters cards index-parameter)
  (FactorIndex vars (assert vord IndexList?)  index))

(: index-to-assignment (Index (Listof Index) -> (Listof Index)))
(define (index-to-assignment idx card)
  
  (: cum-prod ((Listof Index) -> (Listof Index)))
  (define (cum-prod cards)
    (if (pair? cards)
	(let ((init-accum (list (car cards))))
	  (let: loop : (Listof Index) ((cards : (Listof Index) (cdr cards)) 
				       (accum : (Listof Index) init-accum))
 		(if (null? cards)
 		    (cdr accum)
 		    (loop (cdr cards) 
			  (cons (assert (* (car accum) (car cards)) index?)
				accum)))))
	'()))
  
  (let ((radixs (cum-prod card)))
    (let: loop : (Listof Index) ((idx : Index idx) 
				 (radixs : (Listof Index) radixs) 
				 (assign : (Listof Index) '()))
	  (if (null? radixs)
	      (cons idx assign)
	      (let-values (((q r) (quotient/remainder idx (car radixs))))
		(loop r (cdr radixs) (cons q assign)))))))

#| 
Generate all the parameters.

Create a "register" of the size of the number of variables and 
increment modulo the card of each variable.

As each parameter is generated invoke the given function with the current parameter 
and the current index value. 
|#

(: map-parameters ((Vectorof Index) (Index Assignment -> Void) -> Void))
(define (map-parameters cards fn)

  (define: N     : Fixnum (fx- (cardinality-size cards) 1))

  (define: reg   : (Vectorof Index) (make-vector (vector-length cards) #{0 : Index}))
  
  (: inc-and-carry (Fixnum -> Void))
  (define (inc-and-carry idx)
    (let ((n (add1 (vector-ref reg idx))))
      (if (< n (vector-ref cards idx))
          (vector-set! reg idx n)
          (begin
            (vector-set! reg idx 0)
            (inc-and-carry (fx+ idx 1))))))

  (do: : Void  ((i : Fixnum 0 (fx+ i 1)))
       ((fx>= i N) (fn (assert i index?) reg))    
       (fn (assert i index?) (vector-copy reg))
       (inc-and-carry 0)))


(define F1 (mkFactor '(A B) '(3 2) (flvector 0.5 0.8 0.1 0.0 0.3 0.9)))

(define F2 (mkFactor '(B C) '(2 2)
		     (flvector 0.5 0.7 0.1 0.2)))

(define F3 (mkFactor '(A B C) '(3 2 2)
		     (flvector 0.25 0.35 0.08 0.16 0.05  0.07 0.0 0.0 0.15 0.21 0.09 0.18)))

(displayln "===========")
(pretty-print (f* F1 F2))
