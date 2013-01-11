#lang typed/racket

(provide
 dynamic-require/cast)

(require 
 (only-in "types.rkt"
          DynFn-module DynFn-fn)
 (for-syntax
  typed/racket  
  syntax/parse))

(define-syntax (dynamic-require/cast stx)
  (syntax-parse stx 
    [(_ dynfn:id fntype)
     #'(cast (dynamic-require (DynFn-module dynfn) (DynFn-fn dynfn)) fntype)]))
