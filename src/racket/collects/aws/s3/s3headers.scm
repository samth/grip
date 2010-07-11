;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Bravais' Edito Princeps: EBook Tool Suite	    
;; Copyright (C) 2007  Raymond Paul Racine
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

(module s3headers mzscheme
  
  (provide 
   date-header
   content-type
   content-length
   content-md5) 
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Makers of http headers
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  (define (make-header key value)
    (string-append key value))
  
  (define (date-header date)
    (make-header "Date: " date))
  
  (define (content-type mime)
    (make-header "Content-Type: " mime))
  
  (define (content-length len)
    (make-header "Content-Length: " (number->string len)))
  
  (define (content-md5 md5)
    (make-header "Content-MD5: " md5))
  
  )