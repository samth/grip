#lang typed/racket/base

(provide
 RDD
 ;; ;; (struct-out RDDFile)
 ;; ;; (struct-out RDDList)
 (struct-out RDDSeq)
 (struct-out RDDList)
 (struct-out RDDFilter)
 (struct-out RDDMap)
 (struct-out RDDPrint)
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

(struct: (A B) RDDSeq ([xs : (Listof A)]))

(struct: (A B) RDDList ([xs : (Listof A)]))

;;; an RDD is a set of distributed blocks
(struct: (A B) RDDFile ([blocks : (Listof Block)]))

;(struct: (T) RDDList ([block : (Listof T)]))

(struct: (A B) RDDFilter ([parent : (RDD A B)]
                          [filterfn : (B -> Boolean)]))

(struct: (A B) RDDMap ([parent : RDD]
                       [cvt    : (A -> B)]))

(struct: (A B) RDDPrint ([parent : RDD]))

(define-type (RDD A B) ((RDDFilter A B)
                        (RDDMap A B)
                        (RDDFile A B)
                        (RDDSeq A B)
                        (RDDPrint A B)))

;; Build RDD from an input path
(: rdd-text (Location -> RDD))
(define (rdd-text loc)     
  (RDDFile (map (λ: ((p : Location)) (Block (path->complete-path p loc)
                                            (BlockStructure 0 0)))
                (directory-list loc))))

;; (: generate-rdd-block-filename (-> Path))
;; (define (generate-rdd-block-filename)
;;   (make-temporary-file "rktrdd-~a.block" #f rdd-materialization-directory))
