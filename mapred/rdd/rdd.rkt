#lang typed/racket/base

(provide
 ;; ;; (struct-out RDDList)
 (struct-out RDDSeq)
 (struct-out RDDList)
 ;;(struct-out RDDFilter)
 (struct-out RDDMap)
 (struct-out RDDPrint)
 generate-rdd-block-filename
 rddfile-block-count
 )

(require
 racket/pretty
 (only-in httpclient/uri
	  Uri)
 (only-in httpclient/uri/filescheme
          local-path->uri)
 (only-in mapred/types
          Text)
 (only-in racket/file
          make-temporary-file)
 (only-in "../types.rkt"
          Range
          Block BlockSet BlockSet-uri BlockSet-blocks
          RDD RDD-blocksets)          
 (only-in "../config.rkt" 
          DEFAULT-BLOCK-SIZE
          rdd-materialization-directory)
 (only-in "../blockset.rkt"
          blockset-count))

;;(struct: (A B) RDD ([blocks : (Listof BlockSet)]))

(struct: (A B) RDDSeq ([xs : (Listof A)]))

(struct: (A B) RDDList ([xs : (Listof A)]))

;;(struct: (T) RDDList ([block : (Listof T)]))

;;(struct: (A B) RDDFilter ([parent : (RDD A B)]
;;                          [filterfn : (B -> Boolean)]))

(struct: (A B) RDDMap ([parent : RDD]
                       [cvt    : (A -> B)]))

(struct: (A B) RDDPrint ([parent : RDD]))

;; (define-type (RDD A B) ((RDDFilter A B)
;;                         (RDDMap A B)
;;                         ;(RDD A B)
;;                         (RDDSeq A B)
;;                         (RDDPrint A B)))

(: rddfile-block-count (RDD -> Integer))
(define (rddfile-block-count rdd)
  (define: total : Natural 0)
  (for ((blockset (RDD-blocksets rdd)))    
       (set! total (+ total (blockset-count blockset))))
  total)

(: generate-rdd-block-filename (-> Path))
(define (generate-rdd-block-filename)
  (make-temporary-file "rdd-~a.block" #f rdd-materialization-directory))
