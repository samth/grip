;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Ray Racine's TR Library
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

#| Routines applicable to file: schema URIs |#

#lang typed/racket/base

(provide:
 [local-path->uri (Path -> Uri)]
 [local-file-uri? (Uri -> Boolean)]
 [local-file-uri->path (Uri -> Path)])

(require 
 (only-in "../uri.rkt"
          parse-uri
          Uri Uri-scheme Uri-path Uri-authority Authority-host))

(: local-file-uri? (Uri -> Boolean))
(define (local-file-uri? uri)
  (and (string=? (Uri-scheme uri) "file")
       (eq? #f (Uri-authority uri))))

(: local-file-uri->path (Uri -> Path))
(define (local-file-uri->path uri)  
  (string->path (Uri-path uri)))

; no host name
(: local-path->uri (Path -> Uri))
(define (local-path->uri path)
  (assert (parse-uri (string-append "file://" (path->string path)))))

