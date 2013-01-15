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

#| Given a set of blocks merge-sort transform it into an Enumerator of grouped records by a key. i.e.  Reducer ready. |#

#lang typed/racket/base

(provide:
 (parse-block-sort-in-mem (All (D) Uri (Block D) (TextParse D) (Sort D) -> (Listof D)))
 (sorted-blockset-enumerator (All (D E) (BlockSet D) (TextParse D) (Sort D) -> (Enumerator D E))))

(require 
 (only-in prelude/std/prelude
          identityof)
 (only-in httpclient/uri
          Uri)
 (only-in httpclient/uri/filescheme
          local-file-uri->path)
 (only-in io/iteratee/iteratee          
          Iteratee Enumerator icomplete)
 (only-in io/iteratee/enumerators          
          enumerator/select-from-n-lists) 
 (only-in io/iteratee/iteratees
          lister)
 (only-in "../types.rkt"  
          TextParse
          BlockSet BlockSet-uri BlockSet-blocks 
          Block
          Sort)
 (only-in "../rdd/block.rkt"
          enum/text-block))

#| Load an entire partition into memory, create a from memory partition sorted enumerator ready to be fed to a Reducer. |#
#| Obvious constraint here is that a MR partition need to fit into memory. Any MR key's data must fit into a partition, hence into memory. |#

(: parse-block-sort-in-mem (All (D) Uri (Block D) (TextParse D) (Sort D) -> (Listof D)))
(define (parse-block-sort-in-mem uri block parser sorter)  
  (let: ((enum : (Enumerator D (Listof D)) (enum/text-block (local-file-uri->path uri) 
                                                            block parser (identityof D)))
         (iter : (Iteratee D (Listof D)) (lister)))
    (sort (icomplete (enum iter)) sorter)))

(: sorted-blockset-enumerator (All (D E) (BlockSet D) (TextParse D) (Sort D) -> (Enumerator D E)))
(define (sorted-blockset-enumerator blockset parser sorter)
  (define: uri : Uri (BlockSet-uri blockset))
  (define: data : (Listof (Listof D)) 
    (map (λ: ((block : Block))
	     (parse-block-sort-in-mem uri block parser sorter))
         (BlockSet-blocks blockset)))
  (enumerator/select-from-n-lists data sorter))

