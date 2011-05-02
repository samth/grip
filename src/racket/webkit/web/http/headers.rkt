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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Simple helper procedures to create commonly used S3 http REST headers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#lang typed/racket/base

(provide
 Header
 Headers
 ACCEPT USER-AGENT COOKIE DATE HOST 
 CONTENT-TYPE CONTENT-LENGTH CONTENT-MD5
 LOCATION SET-COOKIE
 ;; Header values
 X-WWW-FORM-URLENCODED
 ;; alist process
 headers? empty-headers add-header 
 get-header get-header-value
 get-cookie-header
 ;; Predictes
 x-www-form-urlencoded?  
 ;; make header string from pair (key . value)
 agent-header accept-header date-header
 host-header content-type content-length
 content-md5 location set-cookie)

(define-type Header (Pairof String String))
(define-type Headers (Listof Header))

;; Standard Headers
(define ACCEPT         "Accept")
(define USER-AGENT     "User-Agent")
(define DATE           "Date")
(define HOST           "Host")
(define CONTENT-TYPE   "Content-Type")
(define CONTENT-LENGTH "Content-Length")
(define CONTENT-MD5    "Content-MD5")
(define LOCATION       "Location")
(define COOKIE         "Cookie")
(define SET-COOKIE     "Set-Cookie")

;; Standard Header Values
(define X-WWW-FORM-URLENCODED "application/x-www-form-urlencoded")

;;-;;;;;;;;;;;;;;;;;;;;;;;;;
;; Headers are often alists
;; Manipulation Procedures
;;-;;;;;;;;;;;;;;;;;;;;;;;;;

(: empty-headers Headers)
(define empty-headers '())

(: headers? (Any -> Boolean))
(define (headers? lst)
  (cond
   ((null? lst) #t)
   ((pair? lst)
    (let ((e (car lst)))
      (and (pair? e)
	 (string? (car e)))))
   (else #f)))

(: add-header (String String Headers -> Headers))
(define (add-header k v headers)
  (cons (cons k v) headers))

(: get-header (String Headers -> (Option (Pairof String String))))
(define get-header assoc)

(: get-header-value (String Headers -> (Option String)))
(define (get-header-value header headers)
    (let ((header (get-header header headers)))
      (if header
	 (cdr header)
	 #f)))

;;-;;;;;;;;;;;;;;;;;;;;;;;;
;; Get a http header value
;;-;;;;;;;;;;;;;;;;;;;;;;;;

(: get-cookie-header (Headers -> (Option String)))
(define (get-cookie-header headers)
  (get-header-value COOKIE headers))

;;-;;;;;;;;;;;;;;;;;;;;;;;;;
;; Makers of http headers
;;-;;;;;;;;;;;;;;;;;;;;;;;;;

(: make-header (String String -> String))
(define (make-header key value)
  (string-append key ": " value))

(: accept-header (String -> String))
(define (accept-header value)
  (make-header ACCEPT value))

(: agent-header (String -> String))
(define (agent-header agent-id)
  (make-header USER-AGENT agent-id))

(: date-header (String -> String))
(define (date-header date)
  (make-header  DATE date))

(: host-header (String -> String))
(define (host-header host)
  (make-header HOST host))

(: content-type (String -> String))
(define (content-type mime)
  (make-header CONTENT-TYPE mime))

(: content-length (Integer -> String))
(define (content-length len)
  (make-header CONTENT-LENGTH (assert (number->string len) exact-integer?)))

(: content-md5 (String -> String))
(define (content-md5 md5)
  (make-header CONTENT-MD5 md5))

(: location (String -> String))
(define (location loc)
  (make-header LOCATION loc))

(: set-cookie (String -> String))
(define (set-cookie cookie)
  (make-header SET-COOKIE cookie))

;; Useful predicates
(: x-www-form-urlencoded? (Headers -> Boolean))
(define (x-www-form-urlencoded? headers)
  (let ((header (get-header CONTENT-TYPE headers)))
    (if header
       (string=? (cdr header) X-WWW-FORM-URLENCODED)
	 #f)))
