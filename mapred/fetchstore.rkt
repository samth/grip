#| Fetch and store sets of Blocks in and out of S3|#

#lang typed/racket/base

(provide 
 store-blockset
 fetch-blockset)

(require 
 (only-in "logging.rkt"
          log-mr-error
          log-mr-info)
 (only-in prelude/std/opt
          opt-map)
 (only-in httpclient/uri          
          Authority-host
          Uri  Uri-path Uri-authority)
 (only-in aws/s3/invoke
          S3Response)
 (only-in aws/s3/objects
          [Range S3Range]
          s3-put-file-object
          s3-get-object-to-file)
 (only-in "types.rkt"
          Range Range-sod Range-eod
          Block-range
          BlockSet
          BlockSet-blocks
          BlockSet-uri)
 (only-in "blockset.rkt"
          block-path
          block-local-path
          blockset-local-path
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
            (base-uri (BlockSet-uri blockset))            
            (to-paths (blockset-build-paths blockset target-dir)))                
        (for ([block (BlockSet-blocks blockset)]
              [to   to-paths])
          (let ((from (block-path base-uri block)))                        
            (log-mr-info "S3 get: Bucket ~s, ~s::~s -> ~s" bucket from (Block-range block) to)
            (if (file-exists? to)
                (log-mr-error "Skipping fetch as ~s exists in target directory." to)
                (s3-get-object-to-file bucket (path->string from) to (opt-map (Block-range block) range->s3range))))))))

(: store-blockset (BlockSet Uri -> (Listof S3Response)))
(define (store-blockset blockset s3-uri)    
  (let ((local-path (blockset-local-path blockset))
        (s3-local-path (string->path (Uri-path s3-uri))))
    (for/list ((block (BlockSet-blocks blockset)))
      (let ((in-path (block-local-path local-path block))
            (bucket  (assert (Authority-host (assert (Uri-authority s3-uri)))))
            (out-path (path->string (block-local-path s3-local-path block))))
        (log-mr-info "S3 put: ~s -> Bucket ~s, ~s" in-path bucket out-path)
        (s3-put-file-object in-path bucket out-path)))))  
