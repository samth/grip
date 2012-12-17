#lang typed/racket/base

(provide:
 [partition-iteratee (All (A) (Writer A) (Grouper A) Index -> (Iteratee A (Partition A)))]) 

(require 
 (only-in io/iteratee/iteratee
          Iteratee Stream Continue Done)
 (only-in "types.rkt"
          Writer Grouper Partition
          Block RDDFile)
 (only-in "./rdd/rdd.rkt"         
          generate-rdd-block-filename)
 (only-in "iterfile.rkt"
          OK IOResult))

;; Create an Iteratee which will write a value into one of N partition files.
;; TODO - Buffer and pre-shuffle, and segment each RDDFile into more than one giant block.
(: partition-iteratee (All (A) (Writer A) (Grouper A) Index -> (Iteratee A (Partition A))))
(define (partition-iteratee writer grouper partition-count)
  
  (: block-names (Vectorof Path))
  (define block-names (for/vector: : (Vectorof Path) #:length partition-count ([i (in-range partition-count)])
                        (generate-rdd-block-filename)))
  
  (: open-all-partitions (-> (Vectorof Output-Port)))
  (define (open-all-partitions)
    (for/vector: : (Vectorof Output-Port) #:length partition-count ([name block-names])
      (open-output-file name #:mode 'text)))
  
  (: partition-ports (Vectorof Output-Port))
  (define partition-ports (open-all-partitions))
  
  (: close-all-partitions (-> Void))
  (define (close-all-partitions)
    (for ([p partition-ports])
      (close-output-port p)))
  
  (: write-partitioned-value (A Index -> Void))
  (define (write-partitioned-value s partition)
    (writer s (vector-ref partition-ports partition)))
  
  (: create-rddfile-result (-> (Vectorof (RDDFile A))))
  (define (create-rddfile-result)
    (for/vector: : (Vectorof (RDDFile A)) #:length partition-count ([name block-names])
      ((inst RDDFile A) (list (Block name 0 (file-size name))))))
  
  (: step ((Stream A) -> (Iteratee A (Vectorof (RDDFile A)))))
  (define (step s)    
    (cond
      ([eq? s 'Nothing]
       (Continue step))
      ([eq? s 'EOS]
       (close-all-partitions)
       (Done 'EOS (create-rddfile-result)))
      (else                
       (write-partitioned-value s (modulo (grouper s) partition-count))
       (Continue step))))
  
  (Continue step))