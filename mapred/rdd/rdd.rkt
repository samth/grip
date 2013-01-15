#lang typed/racket/base

(provide
 n-rdd
 n-blockset
 ;; ;; (struct-out RDDList)

 (struct-out RDDSeq)
 (struct-out RDDList)
 ;;(struct-out RDDFilter)
 (struct-out RDDMap)
 (struct-out RDDPrint)
 generate-rdd-block-filename
 rddfile-block-count
 rdd-text)

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

;; Build RDD from an input path
(: rdd-text (case-> (Path -> (RDD Text))
                    (Path Natural -> (RDD Text))))
(define (rdd-text base-dir-path [block-size DEFAULT-BLOCK-SIZE])
  (RDD (list (BlockSet (local-path->uri base-dir-path)
		       (apply append (map (λ: ((file-name : Path)) 
					      (let ((full-path (path->complete-path file-name base-dir-path)))
						(n-block (path->string file-name) (file-size full-path) block-size)))
					  (directory-list base-dir-path)))))))

(: rddfile-block-count (RDD -> Integer))
(define (rddfile-block-count rdd)
  (define: total : Natural 0)
  (for ((blockset (RDD-blocksets rdd)))    
    (set! total (+ total (blockset-count blockset))))
  total)

(: n-block (String Nonnegative-Integer Nonnegative-Integer -> (Listof Block)))
(define (n-block loc n sz)
  (let-values (((bs lb-sz) (quotient/remainder n sz)))
    (let: ((full-blocks : (Listof Block) 
                        (for/list ((b (in-range bs))
                                   #:when (>= b 0)) ;; for type-checker                          
                          (let ((sod (* b sz))
                                (eod (* (add1 b) sz)))
                            (Block loc (Range sod eod))))))
      (if (> lb-sz 0) ;; partial block
          (let* ((sod (* bs sz))
                 (eod (+ sod lb-sz)))
            (cons (Block loc (Range sod eod)) full-blocks))
          full-blocks))))

(: generate-rdd-block-filename (-> Path))
(define (generate-rdd-block-filename)
  (make-temporary-file "rdd-~a.block" #f rdd-materialization-directory))

;; Split a BlockSet into smaller blocksets no larger than N Blocks in size.
;; Does NOT split a Block.
(: n-blockset (BlockSet Natural -> (Listof BlockSet)))
(define (n-blockset blockset n)
  (define uri (BlockSet-uri blockset))
  (let: loop : (Listof BlockSet)  ((blocks     : (Listof Block)'())
				   (blocksets  : (Listof BlockSet) '())
				   (counter    : Natural n)
				   (all-blocks : (Listof Block) (BlockSet-blocks blockset)))
	(if (null? all-blocks)
	    (cons (BlockSet uri blocks) blocksets)
	    (if (zero? counter)
		(loop '() (cons (BlockSet uri blocks) blocksets) n all-blocks)
		(loop (cons (car all-blocks) blocks) blocksets (sub1 counter) (cdr all-blocks))))))

;; Split an RDD into smaller BlockSets no larger than N of them.
(: n-rdd (RDD Natural -> RDD))
(define (n-rdd rdd n)
  (RDD (apply append (map (λ: ((blockset : BlockSet))
			      (n-blockset blockset n))
			  (RDD-blocksets rdd)))))
  
