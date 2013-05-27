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

(provide 
 aif
 orelse)

(require (for-syntax racket/base))

(define-syntax orelse 
  (lambda (stx)
    (syntax-case stx ()
      ((orelse test? exp alt)
       (syntax
	(let ((val exp))
	  (if (test? val)
	     val
	     alt)))))))

(define-syntax aif 
  (lambda (stx) 
    (syntax-case stx () 
      ((aif condition consequent alternative) 
       (with-syntax ((it (datum->syntax (syntax aif) 'it))) 
	 (syntax 
	  (let ((val condition)) 
	    (if val 
	       (let ((it val)) 
		 consequent) 
	       alternative))))))))


