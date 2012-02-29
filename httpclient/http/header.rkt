;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Knozama's Amazon API Library
;; Copyright (C) 2007-2011  Raymond Paul Racine
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
 Header Headers
 make-header make-header-string header->string  empty-headers 
 add-header get-header get-header-value opt-header-value
 agent-header host-header
 date-header content-type content-length content-md5)

(define-type Header (Pairof String String))
(define-type Headers (Listof Header))

(: empty-headers Headers)
(define empty-headers '())

;; Extends Headers with a Header value
(: add-header (String String Headers -> Headers))
(define (add-header k v headers)
  (cons (cons k v) headers))

;; Get a Header from given Headers
(: get-header (String Headers -> (Option (Pairof String String))))
(define get-header assoc)

;; Get the Header value from given Headers
(: get-header-value (String Headers -> (Option String)))
(define (get-header-value header headers)
    (let ((header (get-header header headers)))
      (if header (cdr header) #f)))

(: opt-header-value ((Option Header) -> (Option String)))
(define (opt-header-value hdr)
  (if (pair? hdr)
     (cdr hdr)
     #f))

(: make-header-string (String String -> String))
(define (make-header-string key value)
  (string-append key ":" value))

(: header->string (Header -> String))
(define (header->string hdr)
  (make-header-string (car hdr) (cdr hdr)))

(: make-header (String String -> Header))
(define (make-header hdr val)
  (cons hdr val))

(: host-header (String -> Header))
(define (host-header host)
  (make-header "Host" host))

(: agent-header (String -> Header))
(define (agent-header agent)
  (make-header "User-Agent" agent))

(: date-header (String -> Header))
(define (date-header date)
  (make-header "Date" date))

(: content-type (String -> Header))
(define (content-type mime)
  (make-header "Content-Type" mime))

(: content-length (Integer -> Header))
(define (content-length len)
  (make-header "Content-Length" (number->string len)))

(: content-md5 (String -> Header))
(define (content-md5 md5)
  (make-header "Content-MD5" md5))
