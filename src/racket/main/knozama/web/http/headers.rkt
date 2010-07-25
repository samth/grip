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

#lang racket

(provide
 ;; Headers
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

(define empty-headers '())

(define headers?
  (lambda (lst)
    (cond
     ((null? lst) #t)
     ((pair? lst)
      (let ((e (car lst)))
	(and (pair? e)
	   (string? (car e)))))
     (else #f))))

(define add-header
  (lambda (k v headers)
    (cons (cons k v) headers)))

(define get-header assoc)

(define get-header-value
  (lambda (header-sym headers)
    (let ((header (get-header header-sym headers)))
      (if header
	 (cdr header)
	 #f))))

;;-;;;;;;;;;;;;;;;;;;;;;;;;
;; Get a http header value
;;-;;;;;;;;;;;;;;;;;;;;;;;;

(define get-cookie-header
  (lambda (headers)
    (get-header-value COOKIE headers)))

;;-;;;;;;;;;;;;;;;;;;;;;;;;;
;; Makers of http headers
;;-;;;;;;;;;;;;;;;;;;;;;;;;;

(define (make-header key value)
  (string-append key ": " value))

(define (accept-header value)
  (make-header ACCEPT value))

(define (agent-header agent-id)
  (make-header USER-AGENT agent-id))

(define (date-header date)
  (make-header  DATE date))

(define (host-header host)
  (make-header HOST host))

(define (content-type mime)
  (make-header CONTENT-TYPE mime))

(define (content-length len)
  (make-header CONTENT-LENGTH (number->string len)))

(define (content-md5 md5)
  (make-header CONTENT-MD5 md5))

(define (location loc)
  (make-header LOCATION loc))

(define set-cookie 
  (lambda (cookie)
    (make-header SET-COOKIE cookie)))

;; Useful predicates
(define x-www-form-urlencoded?
  (lambda (headers)
    (let ((header (get-header CONTENT-TYPE headers)))
      (if header
	 (string=? (cdr header) X-WWW-FORM-URLENCODED)
	 #f))))
