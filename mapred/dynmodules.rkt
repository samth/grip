#lang typed/racket/base

;(provide 
 ;;determine-module-path
; dynamic-require-modules)

(require 
 racket/match)

(define-type ModulePath (U Module-Path Resolved-Module-Path Module-Path-Index))
(define-type ModuleProvided (U Symbol False Zero Void))

;(: determine-module-path ((U Symbol Path (Pair Symbol (Listof Symbol))) ->  ModulePath))
;(define (determine-module-path m)
;  (cond
;    ((string? m) (make-resolved-module-path (string->path m)))
;    (else (match m
;            [(list 'file fn) (make-resolved-module-path 
;                              (string->path  fn))]
;            [(cons h '()) (determine-module-path h)]))))
            ;;[(cons h (cons t '())) (cons (determine-module-path h) (determine-module-path t))]))))
    
;(: dynamic-require-modules ((List (U Bytes (List 'file Bytes))
;                                  ModuleProvided) -> (U Any Void)))
;(define (dynamic-require-modules modules)
;  (match-define (list mp sym) modules)
;  (cond 
;    ((symbol? sym)
;     ((inst dynamic-require Any) (determine-module-path mp) sym))
;    ((or (symbol? sym)
;         (eq? #f sym)
;         (void? sym))
;     ((inst dynamic-require Void)
;      (determine-module-path mp) sym))))
