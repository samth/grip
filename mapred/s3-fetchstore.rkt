;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Ray Racine's Munger Library
;; Copyright (C) 2007-2013  Raymond Paul Racine
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#| Fetch and store sets of Blocks in and out of S3|#

#lang typed/racket/base

(provide:
 [s3-put-store-iteratee (All (E) (-> (Iteratee E (BlockSet E))))]
 [fetch-blockset (BlockSet Path -> BlockSet)]
 [store-blockset (BlockSet Uri -> (Listof S3Response))]
 [fetch-blockset-in-mem (BlockSet -> (Listof (U S3Response Text)))]
 [fetch-text-partition-blocks-in-mem (String Natural Natural -> (Listof Text))])

(require 
 racket/pretty
 racket/match
 (only-in "logging.rkt"
          log-mr-error
          log-mr-info)
 (only-in io/iteratee/iteratee
	  Iteratee Stream Continue Done)
 (only-in prelude/std/opt
          opt-map)
 (only-in httpclient/uri
	  parse-uri
          Authority-host
          Uri Uri-authority Uri-path)
 (only-in httpclient/uri/filescheme
          local-path->uri)
 (only-in aws/s3/types
	  Key Key-key Keys-truncated?
	  Keys Keys-objects)
 (only-in aws/s3/uri
          s3-path->prefix
          s3-uri-bucket-and-path)
 (only-in aws/s3/invoke
          S3Response)
 (only-in aws/s3/objects
          [Range S3Range]
          s3-get-object
          s3-put-file-object
          s3-list-bucket-objects
          s3-get-object-to-file)
 (only-in "configuration.rkt"
          job-id
          job-s3-bucket)
 (only-in "types.rkt"
          Text
          Range Range-sod Range-eod
          Block Block-range
          BlockSet BlockSet-blocks BlockSet-uri)
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

(: report-partition-too-big (String -> Void))
(define (report-partition-too-big prefix)
  (log-mr-error "Partition ~s contains more than 999 blocks, unhandled at this time." 
		prefix))

;; ASSUMES less than 1000 partitions
(: s3-blocks-for-partition (String Natural Natural -> (Listof String)))
(define (s3-blocks-for-partition bucket job-id partition-id)
  
  (define prefix (string-append "mr/job-" (number->string job-id) 
				"/mp/" (number->string partition-id) "/"))   
  (define keys (s3-list-bucket-objects bucket prefix "" "" 999))
  (define truncated? (Keys-truncated? keys))
  
  (when truncated?
    (report-partition-too-big prefix))
  
  (map (λ: ((key : Key)) (string-append "/" (Key-key key)))
       (Keys-objects keys)))


;; FIXME We are reading in bytes and then the overhead of converting the entire block
;; into a string.  Need to efficently pipe this right from the HTTP connection right through.
;; START WITH REWRITING THE httpclient lib TO BE iteratee BASED.
(: fetch-s3-block-paths-in-mem (String (Listof String) -> (Listof (U S3Response Text))))
(define (fetch-s3-block-paths-in-mem bucket block-paths)  
  (for/list: : (Listof (U S3Response Text)) ([path block-paths])
	     (log-mr-info "S3 get: Bucket ~s, ~s -> ~s in-memory" bucket bucket path)
	     (let ((result (s3-get-object bucket path)))
	       (if (bytes? result)
		   (bytes->string/latin-1 result)
		   result))))

(: fetch-text-partition-blocks-in-mem (String Natural Natural -> (Listof Text)))
(define (fetch-text-partition-blocks-in-mem bucket job-id partition-id)
  (let ((text-blocks (fetch-s3-block-paths-in-mem bucket 
						  (s3-blocks-for-partition bucket job-id partition-id))))
    (if  (andmap string? text-blocks)
	 text-blocks
	 (error 'fetch-text-partition-blocks-in-mem 
		"One or more S3 fetches for job ~s, partition ~s failed."
		job-id partition-id))))

;; Fetch the given S3 objects and retain them in-memory
(: fetch-blockset-in-mem (BlockSet -> (Listof (U S3Response Text))))
(define (fetch-blockset-in-mem blockset)      
  (let ((bucket (blockset-host blockset))
	(base-uri (BlockSet-uri blockset)))        
    (for/list: : (Listof (U S3Response Text)) ([block : Block (BlockSet-blocks blockset)])
	       (let ((from (block-path base-uri block)))
		 (log-mr-info "S3 get: Bucket ~s, ~s::~s -> ~s" bucket from (Block-range block) "in-memory")
		 (let ((result (s3-get-object bucket (path->string from) (opt-map (Block-range block) range->s3range))))
		   (if (bytes? result)
		       (bytes->string/latin-1 result)
		       result))))))

;; Fetch the given S3 objects and store in the target directory.
(: fetch-blockset (BlockSet Path -> BlockSet))
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
		(s3-get-object-to-file bucket (path->string from) to (opt-map (Block-range block) range->s3range)))))
	(BlockSet (local-path->uri target-dir) (BlockSet-blocks blockset)))))

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


(: s3-put-store-iteratee (All (E) (-> (Iteratee E (BlockSet E)))))
(define (s3-put-store-iteratee)
  
  (: step ((Stream E) -> (Iteratee E (BlockSet E))))
  (define step 
    (λ: ((datum : (Stream E)))
	(match datum
	  ['Nothing (Continue step)]
	  ['EOS     (Done 'EOS (BlockSet (assert (parse-uri "s3://nowhere/tmp/")) '()))]
	  [_        (Continue step)])))

  (Continue step))
