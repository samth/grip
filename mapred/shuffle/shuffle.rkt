#lang typed/racket/base

;; A MR Shuffle is essentially a glorified cluster-wide merge-sort.

(provide
 shuffle-rdd)
 
(require
 (only-in racket/set
          list->set) 
 (only-in "../types.rkt"
          Text RDDFile RDDFile-blocks Block)
 (only-in "merge-sort.rkt"
          blocks-merge-sort)          
 (only-in "../tasktrack.rkt"
          make-tracker Tracker)
 (only-in "../rdd/rdd.rkt"
          generate-rdd-block-filename))          

(define inmem-block-count 16)
(define disc-block-count (/ inmem-block-count 2))

;(: text-block-reader ((Block Text) -> (Listof Text)))
;(define (text-block-reader block)
;  (let: ((enum : (Enumerator Text (Listof Text)) (enum/text-block block read-line))
;         (iter : (Iteratee Text (Listof Text)) (text-list-iteratee)))
;    (icomplete (enum iter))))

;(: text-block-writer ((Listof Text) -> (Block Text)))
;(define (text-block-writer txts)
;  (let ((blockname (generate-rdd-block-filename)))
;    (let: ((enum : (Enumerator Text IOResult) (enumerator/list txts))
;           (iter : TextFileIteratee (iter-text-file blockname #t)))
;      (let ((io-result (icomplete (enum iter))))
;        (Block blockname 0 (file-size blockname))))))

;; Each block is sized to fit comfortably in memory.
;; Sort a block by reading the entire block into memory
;; sorting in-mem, and writing it out.

;; Simple assume single node, multiple Place, merge sort
(: shuffle-rdd (All (D) ((RDDFile D) ((Block D) -> (Listof D)) (D D -> Boolean) (D Output-Port -> Void) -> (RDDFile D))))
(define (shuffle-rdd rddfile formatter sorter writer)
  
  (: inmem-sort-blocks ((Listof (Block D)) -> (Listof (Block D))))
  (define (inmem-sort-blocks blocks)
    
    (: inmem-sort-batch ((Listof (Block D)) Natural -> (Values (Listof (Block D)) (Listof (Block D)))))
    (define (inmem-sort-batch to-do max)
      (let: loop : (Values (Listof (Block D)) (Listof (Block D))) 
        ((batch : (Listof (Block D)) '()) (to-do : (Listof (Block D)) to-do) (cnt : Natural max))
        (if (or (zero? cnt)
                (null? to-do))
            (values batch to-do)
            (loop (cons (car to-do) batch) (cdr to-do) (sub1 cnt)))))
    
    (let: ((todo-queue : (Listof (Block D))
                       (let: loop : (Listof (Block D))
                         ((blocks : (Listof (Block D)) blocks) (processed : (Listof (Block D)) '()))
                         (if (null? blocks)
                             processed
                             (let-values (((batch to-do) (inmem-sort-batch blocks inmem-block-count)))
                               (loop to-do (append (blocks-merge-sort batch formatter sorter writer #f))))))))
      todo-queue))
      
  rddfile)

;(: shuffle-text-rdd ((RDDFile Text) -> (RDDFile Text)))
;(define (shuffle-text-rdd rdd-text)
;  (shuffle-rdd rdd-text 
;               text-block-reader
;               string<?
;               (λ: ((s : Text) (outp : Output-Port))
;                 (displayln s outp))))

;; Sort all the blocks so each block is sorted then merge sorted in chunks of M blocks at a time.
;; Given an RDD of blocks
;; - Let N be the number blocks to sort as a single task.  N being RAM determined.
;; - Partition the RDD into M RDDs such that, (sizeof RDD) / N = M
;; - As M partitioned RDDs can be read into memory, do so, and in memory sort each RDD and then Merge Sort from memory
;;   out to a disc tmp file (blocked) as RDD-sorted.  Each RDD-sorted is of S or less blocks in size.
;; - Place RDD-sorted onto a sorted queue.
;; - Note the in-memory block sort is partitioned across P processors.
;;   But the final output merge sort is single processor.

;; If the size of sorted queue is > 1 take 

