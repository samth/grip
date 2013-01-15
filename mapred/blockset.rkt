;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Ray Racine's Munger API Library
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

#lang typed/racket/base

(provide  
 block-path 
 block-local-path
 blockset-local-path
 blockset-host
 blockset-uris
 blockset-count
 blockset-paths
 blockset-build-paths
 blockset-build-local-paths
 local-file-blockset?)

(require
 (only-in httpclient/uri
          Uri Authority-host Uri-authority Uri-path extend-path)
 (only-in httpclient/uri/filescheme
          local-file-uri? local-file-uri->path)
 (only-in "types.rkt"
          BlockSet BlockSet-uri BlockSet-blocks
          Block Block-name Block-range))

(: blockset-count (BlockSet -> Natural))
(define (blockset-count blockset)
  (length (BlockSet-blocks blockset)))

(: blockset-uris (BlockSet -> (Listof Uri)))
(define (blockset-uris blockset)
  (let ((base (BlockSet-uri blockset)))
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
  (let: ((base : Uri (BlockSet-uri blockset)))
    (map (λ: ((block : Block))
           (block-path base block))
         (BlockSet-blocks blockset))))

(: blockset-block-path (BlockSet Block -> Path))
(define (blockset-block-path blockset block)
  (block-path (BlockSet-uri blockset) block))

(: block-path (Uri Block -> Path))
(define (block-path base block)
  (build-path (Uri-path base) (Block-name block)))

(: block-local-path (Path Block -> Path))
(define (block-local-path path block)
  (build-path path (Block-name block)))

(: blockset-host (BlockSet -> String))
(define (blockset-host blockset)
  (Authority-host (assert (Uri-authority (BlockSet-uri blockset)))))

(: blockset-local-path (BlockSet -> Path))
(define (blockset-local-path blockset)
  (let ((base (BlockSet-uri blockset)))
    (if (local-file-uri? base)        
        (local-file-uri->path base)
        (error 'blockset-local-path 
               "BlockSet base Uri has a host or is not a file scheme: ~s" 
               base))))

(: blockset-build-local-paths (BlockSet -> (Listof Path)))
(define (blockset-build-local-paths blockset)
  (blockset-build-paths blockset (blockset-local-path blockset)))

(: local-file-blockset? (BlockSet -> Boolean))
(define (local-file-blockset? blockset)
  (local-file-uri? (BlockSet-uri blockset)))