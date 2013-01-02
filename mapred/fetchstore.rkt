#lang typed/racket/base

#| Fetch and store Blocks to S3|#

(provide 
 main
 fetch-blockset)

(require
 racket/pretty
 (only-in "logging.rkt"
          log-mr-error
          log-mr-info)
 (only-in prelude/std/opt
          opt-map)
 (only-in httpclient/uri
          parse-uri
          Uri Uri-path)
 (only-in aws/s3/objects
          [Range S3Range]
          s3-get-object-to-file)
 (only-in "types.rkt"
          Range Range-sod Range-eod
          Block Block-name Block-range)
 (only-in "blockset.rkt"
          BlockSet BlockSet-blocks BlockSet-base
          block-path
          blockset-build-paths
          blockset-host))

(: range->s3range (Range -> S3Range))
(define (range->s3range range)
  (S3Range (Range-sod range)
           (Range-eod range)))

;; Fetch the given S3 objects and store in the target directory.
(: fetch-blockset (BlockSet Path -> Void))
(define (fetch-blockset blockset target-dir)
  (if (not (directory-exists? target-dir))
      (error 'fetch-blockset "Target directory: ~s does not exist." target-dir)
      (let ((bucket (blockset-host blockset))
            (base-uri (BlockSet-base blockset))            
            (to-paths (blockset-build-paths blockset target-dir)))                
        (for ([block (BlockSet-blocks blockset)]
              [to   to-paths])
          (let ((from (block-path base-uri block)))                        
            (log-mr-info "S3 get: Bucket ~s, ~s::~s -> ~s" bucket from (Block-range block) to)
            (if (file-exists? to)
                (log-mr-error "Skipping fetch as ~s exists in target directory." to)
                (s3-get-object-to-file bucket (path->string from) to (opt-map (Block-range block) range->s3range))))))))

(define (main)  
  (fetch-blockset (BlockSet (assert (parse-uri "s3://odbd/itemset/input"))
                            (list (Block "part-00110" (Range 0 99))
                                  (Block "part-00111" (Range 100 299))
                                  (Block "part-00112" (Range 100 1099))))
                  (string->path "/tmp")))
