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

#| Evaluate the accuracy of a binary classifier |#

(provide
 ConfusionMatrix)

(provide:
 [new-ConfusionMatrix (case-> (Natural Natural Natural Natural -> ConfusionMatrix)
			      (-> ConfusionMatrix))]
 [record-outcome     (ConfusionMatrix Boolean Boolean -> Void)]
 [hit                (ConfusionMatrix -> Natural)]
 [reject             (ConfusionMatrix -> Natural)]
 [false-alarm        (ConfusionMatrix -> Natural)]
 [miss               (ConfusionMatrix -> Natural)]
 [positive           (ConfusionMatrix -> Natural)]
 [negative           (ConfusionMatrix -> Natural)]
 [predicted-positive (ConfusionMatrix -> Nonnegative-Exact-Rational)]
 [predicted-negative (ConfusionMatrix -> Nonnegative-Exact-Rational)]
 [recall             (ConfusionMatrix -> Nonnegative-Exact-Rational)]
 [fall-out           (ConfusionMatrix -> Nonnegative-Exact-Rational)]
 [accuracy           (ConfusionMatrix -> Nonnegative-Exact-Rational)]
 [specitivity        (ConfusionMatrix -> Nonnegative-Exact-Rational)]
 [precision          (ConfusionMatrix -> Nonnegative-Exact-Rational)]
 [ppv                (ConfusionMatrix -> Nonnegative-Exact-Rational)]
 [npv                (ConfusionMatrix -> Nonnegative-Exact-Rational)]
 [fdr                (ConfusionMatrix -> Nonnegative-Exact-Rational)]
 [mcc                (ConfusionMatrix -> Float)]
 [f1                 (ConfusionMatrix -> Nonnegative-Exact-Rational)])

(require 
 racket/match)

;; Structure to track the true positive, false positive, true negative, false negative.
(struct: ConfusionMatrix ([tp : Natural]
			  [fp : Natural]
			  [tn : Natural]
			  [fn : Natural]) 
	 #:mutable
	 #:transparent)

(: new-ConfusionMatrix (case-> (Natural Natural Natural Natural -> ConfusionMatrix)
			       (-> ConfusionMatrix)))
(define (new-ConfusionMatrix [tp 0] [fp 0] [tn 0] [fn 0])
  (ConfusionMatrix tp fp tn fn))

(: record-outcome (ConfusionMatrix Boolean Boolean -> Void))
(define (record-outcome cm actual predicted)
  (match (cons actual predicted)
	 ([cons #t #t]
	  (set-ConfusionMatrix-tp! cm (add1 (ConfusionMatrix-tp cm))))
	 [(cons #f #f)
	  (set-ConfusionMatrix-tn! cm (add1 (ConfusionMatrix-tn cm)))]
	 [(cons #t #f)
	  (set-ConfusionMatrix-fp! cm (add1 (ConfusionMatrix-fp cm)))]
	 [(cons #f #t)
	  (set-ConfusionMatrix-fn! cm (add1 (ConfusionMatrix-fn cm)))]))

;; TP Correct hits
(: hit (ConfusionMatrix -> Natural))
(define (hit cm)
  (ConfusionMatrix-tp cm))

;; TN Correct rejection
(: reject (ConfusionMatrix -> Natural))
(define (reject cm)
  (ConfusionMatrix-tn cm))

;; FP False alarm, Type I error
(: false-alarm (ConfusionMatrix -> Natural))
(define (false-alarm cm)
  (ConfusionMatrix-fp cm))

;; FN Miss, Type II  error
(: miss (ConfusionMatrix -> Natural))
(define (miss cm)
  (ConfusionMatrix-fn cm))

;; P = TP + FN
(: positive (ConfusionMatrix -> Natural))
(define (positive cm)
  (+ (hit cm) (miss cm)))

;; N = FP + TN
(: negative (ConfusionMatrix -> Natural))
(define (negative cm)
  (+ (false-alarm cm) (reject cm)))

;; Predicted Total Postives
;; P' = TP + FP
(: predicted-positive (ConfusionMatrix -> Nonnegative-Exact-Rational))
(define (predicted-positive cm)
  (+ (hit cm) (false-alarm cm)))

;; Predicted Total Negatives
;; N' = FN + TN
(: predicted-negative (ConfusionMatrix -> Nonnegative-Exact-Rational))
(define (predicted-negative cm)
  (+ (miss cm) (reject cm)))

;; TP/P  sensitivy, hit-rate, recall
;; TPR = TP/P = TP / (TP + FN)
(: recall (ConfusionMatrix -> Nonnegative-Exact-Rational))
(define (recall cm)
  (/ (hit cm) (positive cm)))

;; fall-out, false positive rate
;; FPR = FP/N = FP / (FP + TN)
(: fall-out (ConfusionMatrix -> Nonnegative-Exact-Rational))
(define (fall-out cm)
  (/ (false-alarm cm) (+ (negative cm))))

;; ACC = (TP + TN) / (P + N)
(: accuracy (ConfusionMatrix -> Nonnegative-Exact-Rational))
(define (accuracy cm)
  (/ (+ (hit cm) (reject cm)) (+ (positive cm) (negative cm))))

;; specitivity, SPC, to True Negative Rate
;; TN/N = TN / (FP + TN) = 1 - FPR
(: specitivity (ConfusionMatrix -> Nonnegative-Exact-Rational))
(define (specitivity cm)
  (/ (reject cm) (negative cm)))

;; precision, positive predict value or PPV
;; PPV = TP / (TP + FP)
(: precision (ConfusionMatrix -> Nonnegative-Exact-Rational))
(define (precision cm)
  (/ (hit cm) (+ (hit cm) (false-alarm cm))))

(: ppv (ConfusionMatrix -> Nonnegative-Exact-Rational))
(define ppv precision)

;; negative predict value or NPV
;; NPV = TN /  (TN + FN)
(: npv (ConfusionMatrix -> Nonnegative-Exact-Rational))
(define (npv cm)
  (/ (reject cm) (+ (reject cm) (miss cm))))

;; fdr, false discovery rate
;; FDR = FP / (FP + TP)
(: fdr (ConfusionMatrix -> Nonnegative-Exact-Rational))
(define (fdr cm)
  (/ (false-alarm cm) (+ (false-alarm cm) (hit cm))))

;; Matthews Correlation Coefficient (MCC)
;; MCC = (TP * TN - FP * FN) / SQRT(P * N * P' * N')

(: mcc (ConfusionMatrix -> Float))
(define (mcc cm)
  (real->double-flonum (/ (- (* (hit cm) (reject cm)) (* (false-alarm cm) (miss cm)))
			  (sqrt (* (positive cm) (negative cm) (predicted-positive cm) (predicted-negative cm))))))

;; F1 score
;; F1 = 2 * TP / (P + P') = 2 * TP / (2 * TP + FP + FN)
(: f1 (ConfusionMatrix -> Nonnegative-Exact-Rational))
(define (f1 cm)
  (/ (* 2 (hit cm))
     (+ (positive cm) (predicted-positive cm))))

