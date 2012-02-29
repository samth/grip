;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Knozama's Amazon API Library
;; Copyright (C) 2007,2008,2009,2010  Raymond Paul Racine
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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#lang racket/base

(provide path-split)

(require
 racket/fixnum)

(define path-split
  (lambda (s)
    (let ((limit (string-length s))
	(at-slash (lambda (at)
		    (char=? #\/ (string-ref s at)))))
      (let ((prefix-root
	   (lambda (segs)
	     (if (and (not (zero? limit))
		   (char=? (string-ref s 0) #\/))
		(cons "" segs)
		segs))))
	(if (not (zero? limit))
	   (let loop ((i 0) (start (if (at-slash 0) 1 0)) (segments '()))
	     (cond ((fx>= i limit)
		    (cond 
		     ((fx< start i)
		      (prefix-root (reverse (cons (substring s start i) segments))))
		     ((at-slash (sub1 limit))
		      (prefix-root (reverse (cons "" segments))))
		     (else
		      (prefix-root (reverse segments)))))
		   ((at-slash i)
		    (cond 
		     ((fx> start i)
		      (loop (add1 i) (add1 i) segments))
		     ((fx= start i)
		      (loop (add1 i) (add1 i) (cons "" segments)))
		     (else
		      (loop (add1 i) (add1 i) (cons (substring s start i) segments)))))
		   (else
		    (loop (add1 i) start segments))))
	   s)))))
