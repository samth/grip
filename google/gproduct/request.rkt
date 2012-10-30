;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Knozama's Amazon API Library
;; Copyright (C) 2012  Raymond Paul Racine
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#lang typed/racket/base

(provide
 merge-restrictions
 account-restrict condition-restrict request-fields
 build-restriction merge-restrictions)
 
(require
 (only-in "../../prelude/text/util.rkt"
          weave-string-separator)
 (only-in "../../httpclient/uri/url/param.rkt"
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

(: request-fields (String -> Param))
(define (request-fields path)
  (cons "fields" path))
