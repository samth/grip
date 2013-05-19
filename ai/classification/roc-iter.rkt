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

(provide: 
 [confusion-matrix-sink (-> (Iteratee BinaryResult ConfusionMatrix))])

(require
 (only-in "roc.rkt"
	  BinaryResult ConfusionMatrix 
	  new-ConfusionMatrix record-outcome)
 iteratee)

(: confusion-matrix-sink (-> (Iteratee BinaryResult ConfusionMatrix)))
(define (confusion-matrix-sink)

  (define: cm : ConfusionMatrix (new-ConfusionMatrix))

  (: step ((Stream BinaryResult) -> (Iteratee BinaryResult ConfusionMatrix)))
  (define step 
    (λ: ((s : (Stream BinaryResult)))
	(cond     
	 [(eq? s 'Nothing)
	  (Continue step)]
	 [(eq? s 'EOS)
	  (Done 'EOS cm)]
	 [else 
	  (record-outcome cm s)
	  (Continue step)])))

  (Continue step))
