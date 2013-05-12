;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Ray Racine's Typed Racket Library
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
 deserialize-struct-from-string
 serialize-struct-to-string)

;; Open question here on what is best approach for serialization of struct: lot's of APIs in R.
;; For now simple.  This assumes the structure is prefab or equivalent.

(define-syntax deserialize-struct-from-string
  (syntax-rules ()
    ((_ str type)
     (let ((sin (open-input-string str)))
       (let ((s (cast (read sin) type)))
	 (close-input-port sin)
	 s)))))

(define-syntax serialize-struct-to-string
  (syntax-rules ()
    ((_ struc)
     (let ((outp (open-output-string)))
       (write struc outp)
       (close-output-port outp)
       (get-output-string outp)))))
