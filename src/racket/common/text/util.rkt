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

;; general string / text manipulation

#lang racket/base

(provide weave-string-separator
	 for-lines)

(require (only-in "../type/list.rkt"
		  weave))

;; weave a separator string strictly between every pair of strings in a list
;; (weave '("1" "2" "3") ",") -> "1,2,3"
(define weave-string-separator
  (lambda (sep lst)
    (apply string-append (weave sep lst))))

;; given a file name open and process a file 
;; one line at a time
(define-syntax for-lines
  (syntax-rules (in)
    ((_ line in fname body ...)
     (with-input-from-file fname
       (lambda ()
	 (let loop ((line (get-line (current-input-port))))
	   (if (eof-object? line) (void)
	      (begin 
		body ... 
		(loop (get-line (current-input-port)))))))))))
