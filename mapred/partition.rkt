;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Ray Racine's Munger API Library
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
 [partition-iteratee (All (D) (Writer D) (Partitioner D) (BlockSet D) -> (Iteratee D (BlockSet D)))])

(require 
 (only-in io/iteratee/iteratee
          Iteratee Stream Continue Done)
 (only-in httpclient/uri
          parse-uri)
 (only-in "types.rkt"
          Block-name
          BlockSet BlockSet-uri BlockSet-blocks Range
          Writer Partitioner Partition
          Block RDDFile)
 (only-in "blockset.rkt"
          blockset-count
          blockset-local-path
          blockset-build-local-paths)
 (only-in "./rdd/rdd.rkt"         
          generate-rdd-block-filename)
 (only-in "iterfile.rkt"
          OK IOResult))

#| Iteratee which partitions fed data by the given patition function into multiple target files. |#

;; TODO - Buffer and pre-shuffle, and segment each RDDFile into more than one giant block.
;; If sub sets of target partitions are required use -x extension for each.
(: partition-iteratee (All (D) (Writer D) (Partitioner D) (BlockSet D) -> (Iteratee D (BlockSet D))))
(define (partition-iteratee writer partitioner partition-blockset)
    
  (define: partition-count : Index (assert (blockset-count partition-blockset) index?))
    
  (define: block-paths : (Listof Path) (blockset-build-local-paths partition-blockset))
   
  (: open-all-partitions (-> (Vectorof Output-Port)))
  (define (open-all-partitions)
    (for/vector: : (Vectorof Output-Port) #:length partition-count ([path block-paths])
      (open-output-file path #:mode 'text)))
  
  (: partition-ports (Vectorof Output-Port))
  (define partition-ports (open-all-partitions))
  
  (: close-all-partitions (-> Void))
  (define (close-all-partitions)
    (for ([p partition-ports])
      (close-output-port p)))
      
  (: create-rddfile-result (-> (BlockSet D)))
  (define (create-rddfile-result)
    (define: path : Path (blockset-local-path partition-blockset))
    (let ((blocks (for/list: : (Listof (Block D)) ([block (BlockSet-blocks partition-blockset)])
                    (let ((name (Block-name block)))
                      (Block name (Range 0 (file-size (build-path path name))))))))
      (BlockSet (BlockSet-uri partition-blockset) blocks)))
  
  (: step ((Stream D) -> (Iteratee D (BlockSet D))))
  (define (step s)    
    (cond
      ([eq? s 'Nothing]
       (Continue step))
      ([eq? s 'EOS]
       (close-all-partitions)
       (Done 'EOS (create-rddfile-result)))
      (else                
       (writer s (vector-ref partition-ports (modulo (partitioner s) partition-count)))
       (Continue step))))
  
  (Continue step))