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

#lang typed/racket/base

#| Predict a Logistic Regression Given the Parameters |#

(provide:
 [logistic (Float -> Float)]
 [logistic-inferred (case-> (LogisticCoefficients -> ((Vectorof Float) -> Float))
			    (LogisticCoefficients (Float -> Float) -> ((Vectorof Float) -> Float)))])

(struct: LogisticCoefficients ([β0 : Float]
			       [βn : (Vectorof Float)]) #:transparent)

(: logistic (Float -> Float))
(define (logistic x)
  (/ 1.0 (+ 1.0 (exp (- x)))))

(: identity (Float -> Float))
(define (identity x) x)

(: logistic-inferred (case-> (LogisticCoefficients -> ((Vectorof Float) -> Float))
			     (LogisticCoefficients (Float -> Float) -> ((Vectorof Float) -> Float))))
(define (logistic-inferred coeff [link identity])

  (: check-data-arg ((Vectorof Float) -> Void))
  (define (check-data-arg χs)
    (define error-msg  "Matching coeffient vector length")
    (unless (eqv? (vector-length (LogisticCoefficients-βn coeff)) 
		  (vector-length χs))
	    (raise-argument-error 'logistic-inferred
				  error-msg
				  (vector-length (LogisticCoefficients-βn coeff)))))
  
  (λ: ((χns : (Vectorof Float)))
      (check-data-arg χns)
      (link (for/fold: : Float
		       ([β0 : Float (LogisticCoefficients-β0 coeff)])
		       ([βn : Float (in-vector (LogisticCoefficients-βn coeff))]
			[χn : Float (in-vector χns)])
		       (+ β0 (* βn χn))))))
