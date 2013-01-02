#lang typed/racket/base

(provide 
 (struct-out BlockSet)
 block-path
 blockset-host
 blockset-uris
 blockset-paths
 blockset-build-paths)

(require
 (only-in httpclient/uri
          Uri Authority-host Uri-authority Uri-path extend-path)
 (only-in "types.rkt"
          Block Block-name Block-range))

(struct: BlockSet ([base : Uri]
                   [blocks : (Listof Block)]))

(: blockset-uris (BlockSet -> (Listof Uri)))
(define (blockset-uris blockset)
  (let ((base (BlockSet-base blockset)))
    (map (λ: ((block : Block))
           (extend-path base (Block-name block)))
         (BlockSet-blocks blockset))))

(: blockset-build-paths (BlockSet Path -> (Listof Path)))
(define (blockset-build-paths blockset base)
  (map (λ: ((block : Block))
         (build-path base (Block-name block)))
       (BlockSet-blocks blockset)))

(: blockset-paths (BlockSet -> (Listof Path)))
(define (blockset-paths blockset)
  (let: ((base : Uri (BlockSet-base blockset)))
    (map (λ: ((block : Block))
           (block-path base block))
         (BlockSet-blocks blockset))))

(: blockset-block-path (BlockSet Block -> Path))
(define (blockset-block-path blockset block)
  (block-path (BlockSet-base blockset) block))

(: block-path (Uri Block -> Path))
(define (block-path base block)
  (build-path (Uri-path base) (Block-name block)))

(: blockset-host (BlockSet -> String))
(define (blockset-host blockset)
  (Authority-host (assert (Uri-authority (BlockSet-base blockset)))))
