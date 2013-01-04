;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Ray Racine's MapReduce API Library
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

#| Operate on a block of data via enumeration. |#

#lang typed/racket/base

(provide: 
 [enum/text-block (All (D E A) (Block D) (TextParser D) (Mapper D E) -> (Enumerator E A))]
 [map/text-block (All (D E) (Block D) (TextParser D) (Mapper D E) (Iteratee E (Partition E)) -> (Iteratee E (Partition E)))])
 
(require
 racket/match
 (only-in io/iteratee/iteratee
          Enumerator Iteratee
          Done Continue)
 (only-in "../types.rkt"
          Mapper TextParser Partition
          Text)
 (only-in "../types.rkt"            
          Block Block-name Block-range
          Range-sod Range-eod
          RDDFile))

(: open-text-block (Block -> Input-Port))
(define (open-text-block block)
  (let ((inp (open-input-file (Block-name block)
                              #:mode 'text)))                              
    (let ((range (Block-range block)))
      (when range
        (file-position inp (Range-sod range))        
        (read-line inp)))
    inp))

;; Purely, I'd just build the Enumerator from a parser without use of the mapper.
;; The mapper would be placed in an Enumeratee and the partitioning via and Iteratee.
;; Giving (Enumerator/w parser + Enumeratee/w mapper + Iteratee/w partitioner.
;; But as an optimization step we drop the middle Enumeratee, though I can see 
;; where I would want to bring this back for composability flexibility reasons ... maybe.
(: enum/text-block (All (D E A) (Block D) (TextParser D) (Mapper D E) -> (Enumerator E A)))
(define (enum/text-block block parser mapper)
  (define inp (open-text-block block))
  (define eod (Range-eod (Block-range block)))
  (λ: ((iter : (Iteratee E A)))
    (let loop ((iter iter))
      (match iter
        [(Done _ _) iter]
        [(Continue step)
         (if (> (file-position inp) eod)
             (begin
               (close-input-port inp)
               iter)
             (let ((line (read-line inp)))
               (if (eof-object? line)
                   (begin
                     (close-input-port inp)
                     iter)
                   (let ((data (mapper (parser line)))) ;; Just blowup on bad data, may even be the right-thing-to-do.
                     (loop (step data))))))]))))

(: map/text-block (All (D E) (Block D) (TextParser D) (Mapper D E) (Iteratee E (Partition E)) -> (Iteratee E (Partition E))))
(define (map/text-block block parser mapper partitioner)
  (let: ((enum : (Enumerator E (Vectorof (RDDFile E))) (enum/text-block block parser mapper))) ;; assist TR typer
    (enum partitioner)))
