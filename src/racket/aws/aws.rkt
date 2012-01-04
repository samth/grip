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
 current-aws-credential
 init-aws-credential
 Aws-Credential
 Aws-Credential-account-id
 Aws-Credential-access-key
 Aws-Credential-secret-key
 Aws-Credential-associate-tag
 s3-list-buckets)

(require 
 racket/pretty
 (only-in "credential.rkt"
	  current-aws-credential
	  init-aws-credential
	  Aws-Credential
	  Aws-Credential-account-id
	  Aws-Credential-access-key
	  Aws-Credential-secret-key
	  Aws-Credential-associate-tag)
 (only-in "s3/s3.rkt"
	  s3-list-buckets
	  s3-list-bucket-objects))

(define (test)
  (pretty-print (s3-list-buckets))
  (s3-list-bucket-objects "smbmargin/harvest/spls/201112"))

