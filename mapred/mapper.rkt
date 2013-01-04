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

#| Given a collection of blocks, fetch the blocks from S3, map them into partition blocks, S3 store the partition blocks. |#

#lang typed/racket/base

(require
 racket/pretty
 (only-in "config.rkt"
          working-directory)
 (only-in "types.rkt" 
          Block
          Writer TextParser Partition
          BlockSet BlockSet-blocks
          Mapper Partitioner)
 (only-in io/iteratee/iteratee
          icomplete
          Enumerator Iteratee)
 (only-in "fetchstore.rkt"
          fetch-blockset
          store-blockset)
 (only-in "rdd/block.rkt"
          enum/text-block)
 (only-in "partition.rkt"
          partition-iteratee))

;; Work unit is a successful map/partition of a set of blocks.
;; Succeed or blow-up, there is no middle ground, workflow system deals with a failure.
;; BUT must be idempotent, workflow engine state-machine may invoke multiple times, e.g. retry, cluster failure etc.

(: map-partition/text (All (D E) ((Block D) (BlockSet D) (TextParser D) (Mapper D E) (Writer E) (Partitioner E) (BlockSet D) -> (BlockSet E))))
(define (map-partition/text block blockset parser mapper writer partitioner partition-blockset)
  (fetch-blockset blockset working-directory)      
  
  (: iter (Iteratee E (BlockSet E)))
  (define iter (partition-iteratee writer partitioner partition-blockset))
  
  (for ([block (BlockSet-blocks blockset)])    
    (let: ((enum : (Enumerator E (BlockSet E)) (enum/text-block block parser mapper)))
      (enum iter))) ;; map and partition the block
  
  (icomplete iter)) ;; close out the partition set