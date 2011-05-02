#lang typed/racket

#| Standard functions to work with optional types |#

(provide
 opt-apply-orelse
 opt-map)

(: _opt-map (All (a b) ((Option a) (a -> (Option b)) (-> (Option b))  -> (Option b))))
(define (_opt-map val fn alt)
  (if val
     (fn val)
     (alt)))

(: opt-apply-orelse (All (a b) ((Option a) (a -> b) b -> b)))
(define (opt-apply-orelse val fn alt)
  (if val
     (fn val)
     alt))

(define-syntax opt-map
  (syntax-rules ()
    ((_ val fn alt)
     (_opt-map val fn alt))
    ((_ val fn)
     (opt-map val fn (lambda () #f)))))
