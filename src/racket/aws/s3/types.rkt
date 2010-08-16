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

#lang racket/base

(require net/url
	 aws/misc)

(provide make-s3-bucket
	 s3-bucket?
	 s3-bucket->string
	 make-s3-key
	 s3-key?
	 s3-key->string
	 make-s3-resource
	 s3-resource?
	 s3-resource->string)
  
;;;;;;;;;;;;;
;; S3 Bucket
;;;;;;;;;;;;;

(define (s3-bucket? bucket)
  (path/param? bucket))

(define (make-s3-bucket bucket)
  (make-path/param bucket '()))

(define (s3-bucket->string bucket)
  (path/param-path bucket))

;;;;;;;;;;;
;; S3 key
;;;;;;;;;;;

(define (make-s3-key key-segments)
  (map (λ (seg)
	  (make-path/param seg '()))
       key-segments))

(define (s3-key? key)
  (and (list? key)
     (andmap path/param? key)))

(define (s3-key->string key)
  (weave (map path/param-path key) "/"))

;; ;;;;;;;;;;;;
;; S3 Resource
;; ;;;;;;;;;;;;

(define (make-s3-resource bucket key)    
  (cons bucket key))

(define (s3-resource? res)
  (and (pair? res)
     (s3-bucket? (car res))
     (s3-key? (cdr res))))    

(define (s3-resource->string res)
  (if (null? (cdr res))
     (string-append "/" (path/param-path (car res)))
     (string-append "/" (path/param-path (car res)) "/" (s3-key->string (cdr res)))))
