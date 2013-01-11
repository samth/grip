;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Ray Racine's TR Library
;; Copyright (C) 2007-2013  Raymond Paul Racine
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

#lang typed/racket

(provide 
 identity
 identityof
 %%
 ==>
 vadd1)

(require racket/pretty)

;; create a procedure thunk out of a exp(s)
(define-syntax ==>
  (syntax-rules ()
    ((_ exp ...)
     (lambda ()
       exp ...))))

(define-syntax %%
  (syntax-rules ()
    ((_ exp)
     (let ((tmp exp))
       (pretty-print tmp)
       tmp))))

;; Bump counter for a Vectorof Integer at the given index position.
(define-syntax vadd1
  (syntax-rules ()
    ([bump v idx]
     (vector-set! v idx (add1 (vector-ref v idx))))))

(: identity (All (A) (A -> A)))
(define (identity a) a)

(define-syntax identityof
  (syntax-rules ()
    ((_ type)
     (λ: ((d : type)) d))))

;; (: cons* (All (a) a a * -> (Listof a)))
;; (define (cons* a1 a2 . rest)
;;   (if (null? rest)
;;      (cons a1 a2)
;;      (cons a1 (apply cons* (cons a2 rest)))))
