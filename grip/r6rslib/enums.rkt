#lang typed/racket/base

(require "arithmetic/bitwise.rkt"
	 ;; rnrs/arithmetic/bitwise-6
         (for-syntax typed/racket/base))

;;(require/typed racket/base
;;	       (in-hash (All (a b) (HashTable a b) -> (Sequenceof a b)))))

(provide make-enumeration
         enum-set-universe
         enum-set-indexer
         enum-set-constructor
         enum-set->list
         enum-set-member?
         enum-set-subset?
         enum-set=?
         enum-set-union
         enum-set-intersection
         enum-set-difference
         enum-set-complement
         enum-set-projection
         define-enumeration)

(struct: Universe ((ht : (HashTable Symbol Integer))
		   (syms : (Listof Symbol))))

(struct: Enum-Set ((val : Integer)
		   (uni : Universe)))

;; to accomodate hash-ref signature bug? orelse value must be expression and not atomic
(: false (-> Boolean))
(define false (lambda () #f))

(: make-enumeration-universe ((Listof Symbol) -> Universe))
(define (make-enumeration-universe enums)
  (let: ([ht : (HashTable Symbol Integer) (make-hasheq)])
    (Universe ht
	      (for/list ([s (in-list enums)]
			 #:when (not (hash-ref ht s false)))
			(hash-set! ht s (arithmetic-shift 1 (hash-count ht)))
			s))))

(: make-enumeration ((Listof Symbol) -> Enum-Set))
(define (make-enumeration enum)
  (let ([uni (make-enumeration-universe enum)])
    (Enum-Set (sub1 (arithmetic-shift 1 (hash-count (Universe-ht uni))))
	      uni)))

(: enum-set-universe (Enum-Set -> Enum-Set))
(define (enum-set-universe enum)
  (let ([uni (Enum-Set-uni enum)])
    (Enum-Set (sub1 (arithmetic-shift 1 (hash-count 
					 (Universe-ht uni))))
	      uni)))

(: enum-set-indexer (Enum-Set -> (Symbol -> (Option Integer))))
(define (enum-set-indexer enum)
  (let ([ht (Universe-ht (Enum-Set-uni enum))])
    (lambda (sym)
      (let: ([v : (U Boolean Integer) (hash-ref ht sym false)])
        (if v
	   (bitwise-first-bit-set (assert v exact-integer?))
	   #f)))))

(: enum-set-constructor (Enum-Set -> ((Listof Symbol) -> Enum-Set)))
(define (enum-set-constructor enum)
  (let* ([uni (Enum-Set-uni enum)]
       [ht (Universe-ht uni)])
    (lambda: ((orig-syms : (Listof Symbol)))
      (let: loop : Enum-Set ([syms : (Listof Symbol) orig-syms][val : Integer 0])
	  (cond
	   [(null? syms) (Enum-Set val uni)]
	   [(hash-ref ht (car syms) false)
	    => (lambda (n)
		 (loop (cdr syms) (bitwise-ior val (assert n exact-integer?))))]
	   [else
	    (error 'make-enum-set
		   (if (symbol? (car syms))
		      "symbol not in universe: ~e"
		      "not a symbol: ~e")
		   (car syms))])))))

(: enum-set->list (Enum-Set -> (Listof Symbol)))
(define (enum-set->list enum)
  (let ([v (Enum-Set-val enum)])
    (for/list: : (Listof Symbol) ([sym : Symbol (in-list (Universe-syms (Enum-Set-uni enum)))]
				  [i : Integer (in-naturals)]
				  #:when (not (zero? (bitwise-and (arithmetic-shift 1 i) v))))
	       sym)))

(: enum-set-member? (Symbol Enum-Set -> Boolean))
(define (enum-set-member? sym enum)
  (let ([v (hash-ref (Universe-ht (Enum-Set-uni enum)) sym false)])
    (and v (not (zero? (bitwise-and (assert v exact-integer?) (Enum-Set-val enum)))))))

(: enum-set-subset? (Enum-Set Enum-Set -> Boolean))
(define (enum-set-subset? enum1 enum2)
  #f)

;; in-hash has not type sig I can fathom
  ;; (if (eq? (Enum-Set-uni enum1) (Enum-Set-uni enum2))
  ;;    (= (Enum-Set-val enum1) 
  ;; 	(bitwise-and (Enum-Set-val enum1) (Enum-Set-val enum2)))
  ;;    (let ([ht2 (Universe-ht (Enum-Set-uni enum2))]
  ;; 	 [v1 (Enum-Set-val enum1)]
  ;; 	 [v2 (Enum-Set-val enum2)])
  ;;      (for/fold ([sub? #t])
  ;; 	   ([(key1 val1) (in-hash (Universe-ht (Enum-Set-uni enum1)))]
  ;; 	    #:when sub?)
  ;; 	 (let ([val2 (hash-ref ht2 key1 false)])
  ;; 	   (and val2
  ;; 	      (or (zero? (bitwise-and v1 val1))
  ;; 		 (not (zero? (bitwise-and v2 (assert val2 exact-integer?)))))))))))

(: enum-set=? (Enum-Set Enum-Set -> Boolean))
(define (enum-set=? enum1 enum2)
  (if (eq? (Enum-Set-uni enum1) (Enum-Set-uni enum2))
     (= (Enum-Set-val enum1) (Enum-Set-val enum2))
     (and (enum-set-subset? enum1 enum2)
	(enum-set-subset? enum2 enum1))))


(: check-2-enums/same (Symbol Enum-Set Enum-Set -> Void))
(define (check-2-enums/same who enum1 enum2)
  (unless (eq? (Enum-Set-uni enum1)
               (Enum-Set-uni enum2))
    (error who
           "enumeration sets are not the same enumeration type: ~e ~e"
           enum1 enum2)))

(: enum-set-union (Enum-Set Enum-Set -> Enum-Set))
(define (enum-set-union enum1 enum2)
  (check-2-enums/same 'Enum-Set-union enum1 enum2)
  (Enum-Set (bitwise-ior (Enum-Set-val enum1)
			 (Enum-Set-val enum2))
	    (Enum-Set-uni enum1)))

(: enum-set-intersection (Enum-Set Enum-Set -> Enum-Set))
(define (enum-set-intersection enum1 enum2)
  (check-2-enums/same 'enum-set-intersection enum1 enum2)
  (Enum-Set (bitwise-and (Enum-Set-val enum1)
			 (Enum-Set-val enum2))
	    (Enum-Set-uni enum1)))

(: enum-set-difference (Enum-Set Enum-Set -> Enum-Set))
(define (enum-set-difference enum1 enum2)
  (check-2-enums/same 'enum-set-intersection enum1 enum2)
  (Enum-Set (- (Enum-Set-val enum1)
	       (bitwise-and (Enum-Set-val enum1)
			    (Enum-Set-val enum2)))
	    (Enum-Set-uni enum1)))

(: enum-set-complement (Enum-Set -> Enum-Set))
(define (enum-set-complement enum1)
  (Enum-Set (bitwise-xor (sub1 (arithmetic-shift 
				1 
				(hash-count 
				 (Universe-ht (Enum-Set-uni enum1)))))
			 (Enum-Set-val enum1))
	    (Enum-Set-uni enum1)))

(: enum-set-projection (Enum-Set Enum-Set -> Enum-Set))
(define (enum-set-projection enum1 enum2)
  enum1)

;; in-hash again
;; (let* ([uni2 (Enum-Set-uni enum2)]
;;      [ht2 (Universe-ht uni2)]
;;      [v1 (Enum-Set-val enum1)]
;;      [v2 (Enum-Set-val enum2)])
;;   (Enum-Set
;;    (for/fold ([val 0])
;;        ([(key1 val1) (in-hash (Universe-ht (Enum-Set-uni enum1)))])
;;      (if (zero? (bitwise-and v1 val1))
;; 	  val
;; 	  (let ([val2 (hash-ref ht2 key1 false)])
;; 	    (if val2
;; 	       (bitwise-ior val (assert val2 exact-integer?))
;; 	       val))))
;;    uni2)))

(define-syntax (define-enumeration stx)
  (syntax-case stx ()
    [(_ type-name (sym ...) constructor)
     (let ([syms (syntax->list #'(sym ...))]
	 [ht (make-hasheq)])
       (unless (identifier? #'type-name)
         (raise-syntax-error #f
                             "not an identifier for type name"
                             stx
                             #'type-name))
       (for-each (lambda (sym)
                   (unless (identifier? sym)
                     (raise-syntax-error #f
                                         "not an identifier (to be used as a symbol)"
                                         stx
                                         sym)))
                 syms)
       (unless (identifier? #'constructor)
         (raise-syntax-error #f
                             "not an identifier for type name"
                             stx
                             #'constructor))
       (for ([s (in-list syms)])
	    (unless (hash-ref ht (syntax-e s) #f)
	      (hash-set! ht (syntax-e s)
			 (arithmetic-shift 1 (hash-count ht)))))
       (with-syntax ([(val ...)
                      (map (lambda (s) (hash-ref ht (syntax-e s))) syms)])
	 #'(begin
	     (define enum-universe (make-enumeration-universe (list 'sym ...)))
	     (define-syntax (type-name stx)
	       (syntax-case* stx (sym ...) (lambda (a b) (eq? (syntax-e a) (syntax-e b)))
			     [(_ sym) #''sym]
			     ...
			     [(_ other)
			      (identifier? #'other)
			      (raise-syntax-error #f "not in enumeration" stx #'other)]))
	     (define-syntax (bit-value stx)
	       (syntax-case* stx (sym ...) (lambda (a b) (eq? (syntax-e a) (syntax-e b)))
			     [(_ orig sym) #'val]
			     ...
			     [(_ orig s)
			      (raise-syntax-error #f "not in enumeration" #'orig #'s)]))
	     (...
	      (define-syntax (constructor stx)
		(syntax-case stx ()
		  [(_ s ...)
		   (andmap identifier? (syntax->list #'(s ...)))
		   (with-syntax ([orig stx])
		     #'(Enum-Set (bitwise-ior (bit-value orig s) ...)
				 enum-universe))]))))))]))

