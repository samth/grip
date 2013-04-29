#lang typed/racket

(provide
 opt-undefined?
 opt-defined?
 opt-exists
 opt-getorelse
 opt-getorelse-value
 opt-orelse
 opt-map
 opt-flatmap
 opt-filter
 opt-reject
 opt-foreach
 opt-apply-orelse
 opt-car)


(require 
 "partialfn.rkt")

(: opt-car (All (A) ((Listof A) -> (Option A))))
(define (opt-car lst)
  (if (pair? lst)
      (car lst)
      #f))

;; Is the Option value not defined.
(: opt-undefined? (All (a) (Option a) -> Boolean))
(define (opt-undefined? opt)
  (if opt #f #t))

;; Is the Option value defined.
(: opt-defined? (All (a) (Option a) -> Boolean))
(define (opt-defined? opt) 
  (not (opt-undefined? opt)))

;; Option value exists and it matches the given predicate function
(: opt-exists (All (a) (Option a) (a -> Boolean) -> Boolean))
(define (opt-exists opt pred?)
  (and opt (pred? opt)))

;; The Option's value if defined otherwise the default expression's value
(: opt-getorelse (All (a) (Option a) (-> a) -> a))
(define (opt-getorelse opt exp)
  (if opt opt (exp)))

(: opt-getorelse-value (All (a) (Option a) a -> a))
(define (opt-getorelse-value opt v)
  (if opt opt v))

;; The Option value if defined otherwise the given alternative
(: opt-orelse (All (a) (Option a) (-> (Option a)) -> (Option a)))
(define (opt-orelse opt alt-opt)
  (if opt opt (alt-opt)))

;; Apply the given procedure to the option value if defined.
(: opt-map (All (a b) (Option a) (a -> b) -> (Option b)))
(define (opt-map opt proc)
  (if opt (proc opt) #f))

;; interesting opt-map <-> opt-flatmap
(: opt-flatmap (All (a b) (Option a) (a -> (Option b)) -> (Option b)))
(define (opt-flatmap opt proc)
  (opt-map opt proc))

;; If the Option is defined and the predicate is statisfied 
;; return the value else #f
(: opt-filter (All (a) (Option a) (a -> Boolean) -> (Option a)))
(define (opt-filter opt pred)
  (if (and opt (pred opt)) opt #f))

(: opt-reject (All (a) (Option a) (a -> Boolean) -> (Option a)))
(define (opt-reject opt pred)
   (opt-filter opt (lambda: ((x : a)) (not (pred x)))))

(: opt-foreach (All (T) (Option T) (T -> Void)  -> Void))
(define (opt-foreach opt proc)
  (when opt (proc opt)))

;; Need to wait until PartialFn is working.
;; (: opt-collect (All (a b) (Option a) (PartialFn a b) -> (Option b)))
;; (define (opt-collect opt pfn)
;;   (if (and opt (pfn-defined-at? pfn opt))
;;       (ann opt (Option a));; (pfn opt) ;; FIX ME Racket bug
;;      #f))
     



#| Standard functions to work with optional types |#

;; (provide
;;  opt-apply-orelse
;;  opt-map)

;; (: _opt-map (All (a b) ((Option a) (a -> (Option b)) (-> (Option b))  -> (Option b))))
;; (define (_opt-map val fn alt)
;;   (if val
;;      (fn val)
;;      (alt)))

(: opt-apply-orelse (All (a b) ((Option a) (a -> b) b -> b)))
(define (opt-apply-orelse val fn alt)
  (if val
     (fn val)
     alt))

;; (define-syntax opt-map
;;   (syntax-rules ()
;;     ((_ val fn alt)
;;      (_opt-map val fn alt))
;;     ((_ val fn)
;;      (opt-map val fn (lambda () #f)))))
