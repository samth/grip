#lang typed/racket

(provide PartialFn Fn1 pfn-defined-at?)

(define-type Fn1 (All (a b) a -> b))

(struct: (a b) PartialFn ([fn : (Fn1 a b)] [defined-at? : (a -> Boolean)])
	 #:property prop:procedure (struct-field-index fn))

;; Doesn't work 
(: pfn-defined-at? (All (a b) (PartialFn a b) a -> Boolean))
(define (pfn-defined-at? pf value)
  #t)

;;   ((PartialFn-defined-at? pf) value))

;; All this works just fine

;;(: f (Integer -> String))
;;(define f (lambda (x) "Hello"))

;;(: f-range (Integer -> Boolean))
;;(define (f-range x)
;;  (and (> x 0) (< x 10)))

;;(: pf (PartialFn Integer String))
;;(define pf (PartialFn f f-range))

;;((PartialFn-defined-at? pf) 3)
;;((PartialFn-fn pf) 3) ;; works

;; Doesn't work
;; (pf 3) ;; -> Hello
;; (defined-at? pf 3) ;; -> #t


