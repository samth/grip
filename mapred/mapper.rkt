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

#| Given a collection of blocks, fetch the blocks from S3, map them into partition blocks, S3 store the partition blocks. |#

#lang typed/racket/base

(provide 
 map-partition/text)

(require
 racket/pretty 
 (only-in "logging.rkt"
          log-mr-info)
 (only-in httpclient/uri
          Uri extend-path)
 (only-in httpclient/uri/filescheme
          local-file-uri->path)
 ;(only-in "config.rkt"
 ;         working-directory)
 (only-in aws/s3/invoke
          S3Response)
 (only-in "types.rkt" 
          Block
          Writer TextParser Partition
          BlockSet BlockSet-blocks BlockSet-uri
          Mapper Partitioner)
 (only-in io/iteratee/iteratee
          icomplete
          Enumerator Iteratee)
 (only-in "blockset.rkt"
          blockset-count
          local-file-blockset?)
 (only-in "fetchstore.rkt"
          fetch-blockset
          store-blockset)
 (only-in "rdd/block.rkt"
          enum/text-block)
 (only-in "partition.rkt"
          partition-iteratee))

(: partition-store-uris (Uri Natural -> (Listof Uri)))
(define (partition-store-uris base count)  
  (for/list ([idx (in-range 0 count)])
    (extend-path base (number->string idx))))

;; Work unit is a successful map/partition of a set of blocks.
;; Succeed or blow-up, there is no middle ground, workflow system deals with a failure.
;; BUT must be idempotent, workflow engine state-machine may invoke multiple times, e.g. retry, cluster failure etc.

(: map-partition/text (All (D E) Path Uri (BlockSet D) (TextParser D) (Mapper D E) (Writer E) (Partitioner E) (BlockSet D) -> (Listof (BlockSet E))))
(define (map-partition/text work-dir store-uri blockset parser mapper writer partitioner partition-blockset)
  
  (: working-blockset (BlockSet D))
  (define working-blockset 
    (if (local-file-blockset? blockset)
        blockset
        (fetch-blockset blockset work-dir)))        
  
  (: iter (Iteratee E (BlockSet E)))
  (define iter (partition-iteratee writer partitioner partition-blockset))
  
  (define base-dir (local-file-uri->path (BlockSet-uri working-blockset)))
  
  (for ([block (BlockSet-blocks working-blockset)])    
    (log-mr-info "Mapping block: ~s" block)
    (let: ((enum : (Enumerator E (BlockSet E)) 
                 (enum/text-block base-dir block parser mapper)))
      (enum iter))) 
  
  (let ((blockset (icomplete iter))
        (s3-uris (partition-store-uris store-uri (blockset-count blockset))))
    (let ((local-uri (BlockSet-uri blockset)))
      (let ((store-blocksets (for/list: : (Listof BlockSet) ([block (BlockSet-blocks blockset)])
                               (BlockSet local-uri (list block)))))
        (let ((results (for ([blockset store-blocksets]
                             [s3-uri s3-uris])
                         (store-blockset blockset s3-uri))))
          (pretty-print results))
        store-blocksets))))
