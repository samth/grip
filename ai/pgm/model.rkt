#lang typed/racket/base

(provide:
 [mkModel (String (Listof (U Integer (Pair Symbol Integer))) -> Model)]
 [model-variables (Model -> (Listof Symbol))])
 ;;[variable-cardinalities (Model (Listof Symbol) -> (Listof Integer))]
 ;;[variable-definitions (Model -> (Listof Var))])

(provide
 (struct-out Model)
 variable-cardinalities)

(define-type VarCard (Pair Symbol Integer))

(define-predicate VC? VarCard)

(struct: Model ([name : String]
                [vars : (HashTable Symbol Integer)]) #:transparent)

(: mkModel (String (Listof (U Integer (Pair Symbol Integer))) -> Model))
(define (mkModel name init-vals)

  ;; Generate A ... Z, AA ... ZZ, AAA ... ZZZ, ...
  (: generate-anon-varid (Integer -> Symbol))
  (define (generate-anon-varid id)
    
    (: gen-name (Integer -> String))
    (define (gen-name i)
      (define A-base 65)
      (define letters 26)
      (let-values (((q r) (quotient/remainder i letters)))
	(let ((ch (integer->char (+ r A-base))))
	  (make-string (add1 q) ch))))
  
    (string->symbol (gen-name id)))

  (define model (Model name (make-hash)))  
  (define model-vars (Model-vars model))

  (let loop ((anon 0) (vals init-vals))
    (if (null? vals)
       model
       (let ((init-val (car vals)))
	 (cond
	  ((integer? init-val)
	   (let ((name (generate-anon-varid anon)))
	     (hash-set! model-vars name init-val))
	   (loop (add1 anon) (cdr vals)))
	  ((VC? init-val)
	   (let ((name (car init-val))
	       (card (cdr init-val)))
	     (hash-set! model-vars name card)
	     (loop anon (cdr vals)))))))))

(: variable-cardinalities (Model (Listof Symbol) -> (Listof Integer)))
(define (variable-cardinalities model vars)
  (define (on-fail var) 
    (let ((msg (format "Variable ~a is not defined in model ~s" 
		     var 
		     (Model-name model))))
      (error msg)))
  (let ((var-map (Model-vars model)))
    (map (λ: ((var : Symbol))
 	     (hash-ref var-map var (λ () (on-fail var))))
	 vars)))

(: model-variables (Model -> (Listof Symbol)))
(define (model-variables model)
  (hash-keys (Model-vars model)))

;; (: variable-definitions (Model -> (Listof Var)))
;; (define (variable-definitions model)  
;;   (hash-values (Model-vars model)))

;; ;;  ((inst hash-map Symbol Integer (Pair Symbol Integer)) (Model-vars model) 
;; ;;                                                        (λ (k v)
;; ;;                                                          (cons k v))))


;; (: vars-stride (Model (Listof Symbol) -> (Listof Index)))
;; (define (vars-stride model vars)
;;   (let ((vars (Model-vars model)))
;;     (map (lambda: ((s : Symbol)) (Var-stride (hash-ref vars s))) vars)))
	   
  
