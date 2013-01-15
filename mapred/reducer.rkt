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

(provide
 MergeSort
 reduce-computation
 fetch-build-n-merge-sort-enumerator
 fetch-text-partition-blocks-in-mem)

(require
 racket/pretty
 (only-in "logging.rkt"
          log-mr-info log-mr-error)
 (only-in aws/s3/invoke
          S3Response)
 (only-in io/iteratee/iteratee
          icomplete
          Iteratee Enumerator Enumeratee)
 (only-in io/iteratee/enumeratees
          enumeratee-groupby
          enumeratee-flatmap)
 (only-in io/iteratee/enumerators          
          enumerator/select-from-n-lists)
 (only-in io/iteratee/iteratees
	  lister)
 (only-in httpclient/uri
          parse-uri)
 (only-in "configuration.rkt"
          job-s3-bucket)
 (only-in "types.rkt"
	  S3Partition S3Partition-bucket S3Partition-job-id S3Partition-partition-id
          Reduce Sort TextParse GroupCompare
          Text Write BlockSet)
 (only-in "rdd/block.rkt"
	  enum/text)
 (only-in "s3-fetchstore.rkt"     
	  fetch-text-partition-blocks-in-mem)
 (only-in "file-fetchstore.rkt"
	  file-blockset-iteratee))

(define-type (S3PutStore E)  (Iteratee E (BlockSet E)))
(define-type (BlockSetIteratee D) (Iteratee D (BlockSet D)))
(define-type (MergeSort D E) (Enumerator D (Iteratee (Listof D) (Iteratee E (BlockSet E)))))

(require
 (only-in httpclient/uri/filescheme
	  local-path->uri))

(: reduce-computation (All (D E)
			   (MergeSort D E)
			   (Reduce D E)
			   (GroupCompare D)
			   (BlockSetIteratee E) -> 
			   (BlockSet E)))
(define (reduce-computation enum reduce-fn comparer iter)
  (let*: ((enumT-reduce    : (Enumeratee (Listof D) E (BlockSet E)) (enumeratee-flatmap reduce-fn))
	  (iter-reduce     : (Iteratee (Listof D) (Iteratee E (BlockSet E))) (enumT-reduce iter))	  
	  (enumT-lst       : (Enumeratee D (Listof D) (Iteratee E (BlockSet E))) (enumeratee-groupby comparer))
	  (iter-lst-reduce : (Iteratee D (Iteratee (Listof D) (Iteratee E (BlockSet E)))) (enumT-lst iter-reduce)))
 	 (icomplete (icomplete (icomplete (enum iter-lst-reduce))))))
;	  (BlockSet (local-path->uri (string->path "/tmp")) '())))


;(: reducer (All (D E) (Enumerator D (Iteratee (Listof D) (Iteratee E (BlockSet E)))) (Reducer D E) (GroupComparer D) (Iteratee E (BlockSet E)) -> (BlockSet E)))
;(define (reducer enum reduce-fn comparer iter)
;  (let*: ((enumT-tx  : (Enumeratee (Listof D) E (BlockSet E)) (enumeratee-flatmap reduce-fn))              
;          (iter-reduce : (Iteratee (Listof D) (Iteratee E (BlockSet E))) (enumT-tx iter)))
;    (let*: ((enumT-lst : (Enumeratee  D (Listof D) (Iteratee E (BlockSet E))) (enumeratee-groupby comparer))
;            (iter-lst-reduce : (Iteratee D (Iteratee (Listof D) (Iteratee E (BlockSet E)))) (enumT-lst iter-reduce)))
;      (icomplete (icomplete (icomplete (enum iter-lst-reduce)))))))


#| 
1) Fetch each block in a BlockSet from S3 (all are in a given partition)
2) Read a block into memory.
3) In-mem sort it.
4) Build a n-way merge sort of the blocks
5) Feed the whole thing into the reducer iteratee 
|#
(: fetch-build-n-merge-sort-enumerator  (All (D E) (S3Partition (TextParse D) (Sort D)  -> (MergeSort D E))))
(define (fetch-build-n-merge-sort-enumerator partition parser sorter)

  (: in-mem-parse-sort-partition-text (All (D) (Text (TextParse D) (Sort D) -> (Listof D))))
  (define (in-mem-parse-sort-partition-text text parser sorter)
    (define: enum-text : (Enumerator D (Listof D)) (enum/text text parser))
    (define: iter-list : (Iteratee D (Listof D)) (lister))
    (sort (icomplete (enum-text iter-list)) sorter))
  
  (: fetch-sorted-text-blocks (All (D) S3Partition (TextParse D) (Sort D) -> (Listof (Listof D))))
  (define (fetch-sorted-text-blocks partition parser sorter)
    (map (λ: ((text-block : Text))
	     (in-mem-parse-sort-partition-text text-block parser sorter))	 
	 (fetch-text-partition-blocks-in-mem (S3Partition-bucket partition)
					     (S3Partition-job-id partition)
					     (S3Partition-partition-id partition))))
  
  (enumerator/select-from-n-lists (fetch-sorted-text-blocks partition parser sorter) sorter))
    

;(: reduce-partition/text (All (D E) S3Partition (TextParse D) (Sort D) (Reduce D E) (GroupCompare E) (Write E) (S3PutStore E) -> (BlockSet E)))
;(define (reduce-partition/text s3-partition parser sorter reducer comparer writer s3-putter)
;  (define: enum : (MergeSort  D E) (fetch-build-n-merge-sort-enumerator s3-partition parser sorter))
;  (reduce-computation enum reducer comparer s3-putter))

