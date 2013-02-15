#lang typed/racket/base

(provide
 (struct-out CSeries)
 new-CSeries)
;;writer-CSeries)

(provide:
 [CSeries->SIndex (CSeries -> SIndex)]
 [cseries-count   (CSeries -> Nonnegative-Integer)])

(require 
 (only-in "series.rkt"
          SIndex
          LabelIndex))

;; Categorical Series
;; Encoded as an array of integer values with an associated nominal.
;; Custom Structure Writer 
;; See 12.8 Printer Extensions in Racket doc.
(: writer-CSeries (CSeries Output-Port Boolean -> Void))
(define (writer-CSeries series port mode)  
  (let* ([data (CSeries-data series)]
         [nominals (CSeries-nominals series)]
         [len (vector-length data)])
    (do ([i 0 (add1 i)])
	((>= i len) (void))
      (displayln  (vector-ref nominals (vector-ref data i))))))

(struct: CSeries LabelIndex ([data     : (Vectorof Index)] 
			     [nominals : (Vectorof Symbol)])
	 #:transparent)
;; #:methods gen:custom-write [(define write-proc writer-CSeries)])

(: new-CSeries ((Vectorof Symbol) -> CSeries))
(define (new-CSeries nominals)

  (: nominal-code (HashTable Symbol Index))
  (define nominal-code (make-hash))

  (define len (vector-length nominals))

  (: data (Vectorof Index))
  (define data (make-vector len 0))

  (: make-nominal-vector (-> (Vectorof Symbol)))
  (define (make-nominal-vector)
    (define nominals (make-vector (hash-count nominal-code) 'Null))
    (hash-for-each nominal-code
		   (λ: ((nom : Symbol) (idx : Index))
		       (vector-set! nominals idx nom)))
    nominals)


  (let: loop : CSeries ((idx : Natural 0) (code : Index 0))
	(if (>= idx len)
	    (CSeries #f data (make-nominal-vector))
	    (let ((nom (vector-ref nominals idx)))
	      (if (hash-has-key? nominal-code nom)
		  (begin
		    (vector-set! data idx (hash-ref nominal-code nom))
		    (loop (add1 idx) code))
		  (begin
		    (hash-set! nominal-code nom code)
		    (vector-set! data idx code)
		    (loop (add1 idx) (assert (add1 code) index?))))))))



(: CSeries->SIndex (CSeries -> SIndex))
(define (CSeries->SIndex cs)
  
  (: sindex SIndex)  
  (define sindex (make-hash))
  
  (let* ((noms (CSeries-nominals cs))
         (len (vector-length noms)))
    (do ([i 0 (add1 i)])
	([>= i len] sindex)
      (when (index? i)     
        (hash-set! sindex (vector-ref noms i) i)))))

(: cseries-count (CSeries -> Nonnegative-Integer))
(define (cseries-count series)
  (vector-length (CSeries-data series)))
