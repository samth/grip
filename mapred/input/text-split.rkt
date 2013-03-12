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
 [rdd-text (case-> (Path -> (RDD Text))
		   (Path Natural -> (RDD Text)))])

(require
 (only-in httpclient/uri/filescheme
	  local-path->uri)
 (only-in "../config.rkt"
	  DEFAULT-BLOCK-SIZE)
 (only-in "../types.rkt"
	  Text RDD BlockSet)
(only-in "split.rkt"
	 n-block))

;; Build RDD from an input path
(: rdd-text (case-> (Path -> (RDD Text))
		    (Path Natural -> (RDD Text))))
(define (rdd-text base-dir-path [block-size DEFAULT-BLOCK-SIZE])
  (RDD (list (BlockSet (local-path->uri base-dir-path)
		       (apply append (map (λ: ((file-name : Path)) 
					      (let ((full-path (path->complete-path file-name base-dir-path)))
						(n-block (path->string file-name) (file-size full-path) block-size)))
					  (directory-list base-dir-path)))))))


