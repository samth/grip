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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#lang typed/racket/base

(provide 
 ~a ~r)

(define-type SignString (U String (List String String)))
(define-type SignTable (List SignString SignString SignString))

(require/typed racket/format
 (~a (String 
      [#:separator String]
      [#:width Natural]
      [#:max-width Natural]
      [#:min-width Natural]
      [#:limit-marker String]
      [#:align (U 'left 'right 'center)]
      [#:pad-string String]
      [#:left-pad-string String]
      [#:right-pad-string String] -> String))
 
 (~r (Real [#:sign (Option (U '+ '++ 'parens SignTable))]
	   [#:base Natural] ;; 2 - 36
	   [#:precision (U Natural (List '= Natural))]
	   [#:notation (U 'positional 'exponential)]
	   [#:format-exponent (Option (U String (Integer -> String)))]
	   [#:min-width Natural]
	   [#:pad-string String] -> String)))
