;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Knozama's Amazon API Library
;; Copyright (C) 2012  Raymond Paul Racine
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
 Key Key? Key-name Key-type
 KeyVal KeyVal? KeyVal-value KeyVal-type
 Item Item? Item-name Item-value Item-type
 ddbtype-code ddbtype-symbol DDBType)

(define-type DDBType (U 'String 'Number))

(: ddbtype-code (DDBType -> String))
(define (ddbtype-code type)
  (case type
    ((String) "S")
    ((Number) "N")))

(: ddbtype-symbol (DDBType -> Symbol))
(define (ddbtype-symbol type)
  (case type
    ((String) 'S)
    ((Number) 'N)))

(struct: Key ([name : String]
	      [type : DDBType]) #:transparent)

(struct: KeyVal ([value : String] [type : DDBType]) #:transparent)

(struct: Item ([name : String] [value : String] [type : DDBType]) #:transparent)
