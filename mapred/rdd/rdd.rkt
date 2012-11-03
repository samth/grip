#lang typed/racket/base

(provide
 RDD
 (struct-out RDDFile)
 (struct-out RDDList)
 (struct-out RDDFilter)
 (struct-out RDDMap)
 rdd-text)

(require
 (only-in mapred/types
          Text)
 (only-in racket/file
          make-temporary-file)
 (only-in "types.rkt"
          Block
          BlockStructure
          Location)     
 (only-in "../config.rkt" 
          rdd-materialization-directory))

;(define-type (RDDSeq T) (Sequenceof T))

;;; an RDD is a set of distributed blocks
(struct: (T) RDDFile ([blocks : (Listof Block)]))

(struct: (T) RDDList ([block : (Listof T)]))

(struct: (T V) RDDFilter ([parent : (RDD T V)]
                          [filter-fn : (T -> Boolean)]))

(struct: (T V) RDDMap ([parent : (RDD T V)]
                       [cvt    : (T -> V)]))

(define-type (RDD T V) (U (RDDFile T)
                          (RDDList T)
                          (RDDFilter T V)
                          (RDDMap T V)))

;; Build RDD from an input path
(: rdd-text (Location -> (RDD Text Nothing)))
(define (rdd-text loc)     
  (RDDFile (map (λ: ((p : Location)) (Block (path->complete-path p loc)
                                            (BlockStructure 0 0)))
                (directory-list loc))))

(: generate-rdd-block-filename (-> Path))
(define (generate-rdd-block-filename)
  (make-temporary-file "rktrdd-~a.block" #f rdd-materialization-directory))