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

#lang typed/racket/base

(provide path-split)

(define-syntax ++
  (syntax-rules ()
    ((++ x)
     (+ x 1))))

(define-syntax --
  (syntax-rules ()
    ((-- x)
     (- x 1))))

(: path-split (String -> (Listof String)))
(define (path-split s)

  (: limit Index)
  (define limit (string-length s))
  
  (: at-slash (Integer -> Boolean))
  (define (at-slash at)
    (char=? #\/ (string-ref s at)))
  
  (: prefix-root ((Listof String) -> (Listof String)))
  (define (prefix-root segs)
    (if (and (not (zero? limit))
	     (char=? (string-ref s 0) #\/))
	(cons "" segs)
	segs))
  
  (if (> limit 0)
      (begin
	(let: loop : (Listof String) 
	      ((idx : Integer 0)
	       (start : Integer (if (at-slash 0) 1 0)) 
	       (segments : (Listof String) '()))
	      (cond ((>= idx limit)
		     (cond 
		      ((< start idx)
		       (prefix-root (reverse (cons (substring s start idx) segments))))
		      ((at-slash (-- limit))
		       (prefix-root (reverse (cons "" segments))))
		      (else
		       (prefix-root (reverse segments)))))
		    ((at-slash idx)
		     (cond 
		      ((> start idx)
		       (loop (++ idx) (++ idx) segments))
		      ((= start idx)
		       (loop (++ idx) (++ idx) (cons "" segments)))
		      (else
		       (loop (++ idx) (++ idx) (cons (substring s start idx) segments)))))
		    (else
		     (loop (++ idx) start segments)))))
      (list s)))
