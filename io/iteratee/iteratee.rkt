#lang typed/racket/base

(provide
 iseq icomplete 
 Iteratee Stream
 (struct-out Continue)
 (struct-out Done)
 Enumerator eseq
 Enumeratee)

(require racket/match)

(define-type (Stream D) (U D 'Nothing 'EOS))

(define-type (Iteratee D A) (U (Done D A) (Continue D A)))

(struct: (D A) Done ([stream : (Stream D)]
                     [accum : A]))

(struct: (D A) Continue ([step : ((Stream D) -> (Iteratee D A))]))	 

(: icomplete (All (D A) (Iteratee D A) -> A))
(define (icomplete iter)
  (match iter
    [(Done _ accum)  accum]
    [(Continue step) (icomplete (step 'EOS))]))

(: iseq (All (D A B) ((Iteratee D A) (A -> (Iteratee D B)) -> (Iteratee D B))))
(define (iseq iter fn)
  (match iter
    [(Done d a) (fn a)]
    [(Continue step) (Continue (λ: ((d : (Stream D)))
				   (iseq (step d) fn)))]))

(define-type (Enumerator D A) ((Iteratee D A) -> (Iteratee D A)))

(: eseq (All (D A) (Enumerator D A) (Enumerator D A) -> (Enumerator D A)))
(define (eseq e1 e2)
  (lambda (iter)
    (e2 (e1 iter))))

(define-type (Enumeratee O I A) ((Iteratee I A) -> (Iteratee O (Iteratee I A))))
