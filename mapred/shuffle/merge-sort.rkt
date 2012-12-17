#lang typed/racket/base

(provide 
 ;;inmemory-sort-blocks-and-mergesort-to-disc 
 blocks-merge-sort) 

(require 
 (only-in io/iteratee/iteratee          
          Iteratee Enumerator icomplete)
 (only-in io/iteratee/enumerators          
          enumerator/select-from-n-lists) 
 (only-in mapred/iterfile
          iter-block)     
 (only-in "../types.rkt"
          Block RDDFile
          BlockFormatter Sorter Writer )
 (only-in "../rdd/rdd.rkt"
          generate-rdd-block-filename))

(: merge-sort-rdd (All (D) (RDDFile D) -> (RDDFile D)))
(define (merge-sort-rdd rddfile)
  rddfile)

(: inmem-block-sort (All (D) (Block D) (BlockFormatter D) (Sorter D) -> (Listof D)))
(define (inmem-block-sort block formatter sorter)
  (sort (formatter block) sorter))

(: inmemory-sort-blocks-and-mergesort-to-disc (All (D) (Listof (Block D)) (BlockFormatter D) (Sorter D) (Writer D) -> (Listof (Block D))))
(define (inmemory-sort-blocks-and-mergesort-to-disc blocks formatter sorter writer)
  (let: ((sorted-blocks : (Listof (Listof D))
                        (map (λ: ((block : (Block D)))
                               (inmem-block-sort block formatter sorter))
                             blocks)))    
    (merge-sort-inmem-blocks-to-disc sorted-blocks sorter writer)))

(: merge-sort-blocks-to-disc (All (D) (Listof (Block D)) (BlockFormatter D) (Sorter D) (Writer D) -> (Listof (Block D))))
(define (merge-sort-blocks-to-disc blocks formatter sorter writer)
  (let ((lblocks (map formatter blocks)))
    (merge-sort-inmem-blocks-to-disc lblocks sorter writer)))

(: merge-sort-inmem-blocks-to-disc (All (D) (Listof (Listof D)) (Sorter D) (Writer D) -> (Listof (Block D))))
(define (merge-sort-inmem-blocks-to-disc lst-data sorter writer)
  (let: ((enum : (Enumerator D (Listof (Block D))) (enumerator/select-from-n-lists lst-data sorter))
         (iter : (Iteratee D (Listof (Block D)))   (iter-block (generate-rdd-block-filename) writer)))
    (icomplete (enum iter))))

;; Given a list of blocks, and the initial sort status, merge sort the blocks into a list of blocks.
(: blocks-merge-sort (All (D) (Listof (Block D)) (BlockFormatter D) (Sorter D) (Writer D) Boolean -> (Listof (Block D))))
(define (blocks-merge-sort blocks formatter sorter writer block-sorted?)
  (let: ((sorted-blocks : (Listof (Block D))
                        (if block-sorted? 
                            blocks
                            (inmemory-sort-blocks-and-mergesort-to-disc blocks formatter sorter writer))))
    sorted-blocks))