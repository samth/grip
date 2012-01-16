#lang typed/racket/base

(provide
 account-restrict condition-restrict
 build-restriction merge-restrictions)
 
(require
 racket/pretty
 (only-in (planet knozama/common:1/text/util)
	  weave-string-separator)
 (only-in (planet knozama/webkit:1/web/uri/url/param)
	  Param))

(: build-restriction (String (Listof String) -> Param))
(define (build-restriction key values)
  (cons "restrictBy" (string-append key "=" (weave-string-separator "|" values))))

(: merge-restrictions ((Listof Param) -> (Option Param)))
(define (merge-restrictions params)
  (let ((ps (filter (lambda: ((p : Param)) (string=? "restrictBy" (car p))) params)))
    (if (pair? ps)
       (let ((rs (map (lambda: ((p : Param)) (cdr p)) ps)))
	 (let ((value (weave-string-separator "," rs)))
	   (cons "restrictBy" value)))
       #f)))

(: condition-restrict ((Listof String) -> Param))
(define (condition-restrict condition)
  (build-restriction "condition" condition))

(: account-restrict ((Listof String) -> Param))
(define (account-restrict ids)
  (build-restriction "accountId" ids))
