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

#lang typed/racket/base

(provide: 
 [s3-split-bucket-prefix (String String String Natural Natural -> (Values BlockSet String))])

(require
 racket/pretty
 (only-in httpclient/uri
	  Uri)
 (only-in aws/s3/types
	  Key Key-key Key-size
	  Keys Keys-objects)
 (only-in aws/s3/s3-uri
	  new-s3-uri)
 (only-in aws/s3/objects
	  s3-list-bucket-objects)
 (only-in "../types.rkt"
	  Block BlockSet)
 (only-in "split.rkt"
	  n-block))

#| Basic Splitting |#
#|
Focus on 3 Storage Options.
1) A set of zipfiles.
2) A set of LZO files.
3) A set of textual files.
|#

;; Max number of object to fetch in a single s3 listing call.
(define max-listing-size 10)

(: s3-split-bucket-prefix (String String String Natural Natural -> (Values BlockSet String)))
(define (s3-split-bucket-prefix bucket prefix marker min-splits max-split-size)
  (let ((keys (s3-list-bucket-objects bucket prefix "" marker max-listing-size)))
    (let: loop : (Values BlockSet String) 
	  ([keys : (Listof Key) (Keys-objects keys)] 
	   (blocks : (Listof Block) '()) 
	   (last-key : String ""))
	  (if (or (null? keys) 
		  (>= (length blocks) min-splits))
	      (values 
	       (BlockSet (new-s3-uri bucket prefix) blocks)
	       last-key)
	      (let* ((key (car keys))
		     (name (Key-key key))
		     (size (Key-size key)))
		(if (> size 0)		   
		    (let ((new-blocks (n-block name size max-split-size)))
		      (loop (cdr keys)
			    (append new-blocks blocks) 
			    name))
		    (loop (cdr keys) blocks name)))))))
