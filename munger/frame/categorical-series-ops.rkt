;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Ray Racine's Data Munger Library
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
 [cseries-head (CSeries [#:rows Index] -> Void)]
 [cseries-unique (CSeries -> CSeries)] 
 [cseries-append (CSeries CSeries -> CSeries)])

(require 
 (only-in type/format
	  ~a)
 (only-in racket/vector
	  vector-copy)
 (only-in "categorical-series.rkt"
	  CSeries CSeries-nominals cseries-count cseries-referencer)
 (only-in "categorical-series-builder.rkt"
	  CSeriesBuilder
	  new-CSeriesBuilder
	  append-CSeriesBuilder
	  complete-CSeriesBuilder))

(: cseries-append (CSeries CSeries -> CSeries))
(define (cseries-append csa csb)

  (define builder (new-CSeriesBuilder (assert (+ (cseries-count csa)
						 (cseries-count csb))
					      index?)))

  (: append-cseries (CSeries -> Void))
  (define (append-cseries cs)
    (define cs-cnt (cseries-count cs))
    (define cref (cseries-referencer cs))
    (do ([i 0 (add1 i)])
	((>= i cs-cnt))
      (append-CSeriesBuilder builder (cref (assert i index?)))))
  
  (append-cseries csa)
  (append-cseries csb)
  (complete-CSeriesBuilder builder))


(: cseries-unique (CSeries -> CSeries))
(define (cseries-unique cseries)
  (define noms (vector-copy (CSeries-nominals cseries)))
  
  (define len (vector-length noms))
  (define: data : (Vectorof Index) (make-vector len 0))

  (do ([i 0 (add1 i)])
      ((>= i len) (CSeries #f data noms))
    (let: ((i : Index (assert i index?)))
	  (vector-set! data i i))))

(define default-cseries-rows 10)

(: cseries-head (CSeries [#:rows Index] -> Void))
(define (cseries-head cseries #:rows [rows default-cseries-rows])
  (define cref (cseries-referencer cseries))
  (let ((rows (min rows (cseries-count cseries))))
    (do ([i 0 (add1 i)])
	((>= i rows) (displayln ""))
      (display (~a (string-append "[" (number->string i) "]") 
		   #:width 5 #:align 'left))
      (displayln (~a (symbol->string (cref (assert i index?))) 
		     #:align 'left)))))
