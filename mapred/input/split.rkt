;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Ray Racine's Munger Library
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

#|Utilitities to split large data sources into small Blocks |#

(provide:
 [n-block (String Nonnegative-Integer Nonnegative-Integer -> (Listof Block))]
 [n-rdd (RDD Natural -> RDD)]
 [n-blockset (BlockSet Natural -> (Listof BlockSet))])

(require
 (only-in "../types.rkt"
	  RDD RDD-blocksets
	  Block BlockSet BlockSet-uri BlockSet-blocks
	  Range))

(: n-block (String Nonnegative-Integer Nonnegative-Integer -> (Listof Block)))
(define (n-block loc object-sz block-sz)
  (let-values (((bs lb-sz) (quotient/remainder object-sz block-sz)))
    (let: ((full-blocks : (Listof Block) 
                        (for/list ((block-num (in-range bs))
                                   #:when (>= block-num 0)) ;; for type-checker
				  (let ((sod (* block-num block-sz))
					(eod (* (add1 block-num) block-sz)))
				    (Block loc (Range sod eod))))))
	  (if (> lb-sz 0) ;; partial block
	      (let* ((sod (* bs block-sz))
		     (eod (+ sod lb-sz)))
		(cons (Block loc (Range sod eod)) full-blocks))
	      full-blocks))))

;; Split a BlockSet into smaller blocksets no larger than N Blocks in size.
;; Does NOT split a Block.
(: n-blockset (BlockSet Natural -> (Listof BlockSet)))
(define (n-blockset blockset n)
  (define uri (BlockSet-uri blockset))
  (let: loop : (Listof BlockSet)  ((blocks     : (Listof Block)'())
				   (blocksets  : (Listof BlockSet) '())
				   (counter    : Natural n)
				   (all-blocks : (Listof Block) (BlockSet-blocks blockset)))
	(if (null? all-blocks)
	    (cons (BlockSet uri blocks) blocksets)
	    (if (zero? counter)
		(loop '() (cons (BlockSet uri blocks) blocksets) n all-blocks)
		(loop (cons (car all-blocks) blocks) blocksets (sub1 counter) (cdr all-blocks))))))

;; Split an RDD into smaller BlockSets no larger than N of them.
(: n-rdd (RDD Natural -> RDD))
(define (n-rdd rdd n)
  (RDD (apply append (map (λ: ((blockset : BlockSet))
			      (n-blockset blockset n))
			  (RDD-blocksets rdd)))))

