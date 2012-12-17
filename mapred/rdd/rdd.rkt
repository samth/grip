#lang typed/racket/base

(provide
 RDD 
 ;; ;; (struct-out RDDList)
 (struct-out RDDSeq)
 (struct-out RDDList)
 (struct-out RDDFilter)
 (struct-out RDDMap)
 (struct-out RDDPrint)
 generate-rdd-block-filename
 rddfile-block-count
 rdd-text)

(require
 racket/pretty
 (only-in mapred/types
          Text)
 (only-in racket/file
          make-temporary-file)
 (only-in "../types.rkt"
          Block RDDFile RDDFile-blocks     
          Location)     
 (only-in "../config.rkt" 
          DEFAULT-BLOCK-SIZE
          rdd-materialization-directory))

(struct: (A B) RDDSeq ([xs : (Listof A)]))

(struct: (A B) RDDList ([xs : (Listof A)]))


;(struct: (T) RDDList ([block : (Listof T)]))

(struct: (A B) RDDFilter ([parent : (RDD A B)]
                          [filterfn : (B -> Boolean)]))

(struct: (A B) RDDMap ([parent : RDD]
                       [cvt    : (A -> B)]))

(struct: (A B) RDDPrint ([parent : RDD]))

(define-type (RDD A B) ((RDDFilter A B)
                        (RDDMap A B)
                        ;(RDDFile A B)
                        (RDDSeq A B)
                        (RDDPrint A B)))

;; Build RDD from an input path
(: rdd-text (case-> (Location -> (RDD Nothing Text))
                    (Location Nonnegative-Integer -> (RDD Nothing Text))))
(define (rdd-text base-dir-path [block-size DEFAULT-BLOCK-SIZE])
  (RDDFile (apply append (map (λ: ((file-name : Location)) 
                                (let ((full-path (path->complete-path file-name base-dir-path)))                                  
                                  (n-block full-path (file-size full-path) block-size)))
                              (directory-list base-dir-path)))))

(: rddfile-block-count (RDDFile -> Integer))
(define (rddfile-block-count rdd)
  (length (RDDFile-blocks rdd)))

(: n-block (Location Nonnegative-Integer Nonnegative-Integer -> (Listof Block)))
(define (n-block loc n sz)
  (let-values (((bs lb-sz) (quotient/remainder n sz)))
    (let: ((full-blocks : (Listof Block) 
                        (for/list ((b (in-range bs))
                                   #:when (>= b 0)) ;; for type-checker                          
                          (let ((sod (* b sz))
                                (eod (* (add1 b) sz)))
                            (Block loc sod eod)))))
      (if (> lb-sz 0) ;; partial block
          (let* ((sod (* bs sz))
                 (eod (+ sod lb-sz)))
            (cons (Block loc sod eod) full-blocks))
          full-blocks))))

(: generate-rdd-block-filename (-> Path))
(define (generate-rdd-block-filename)
  (make-temporary-file "rdd-~a.block" #f rdd-materialization-directory))
