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
 aws-auth-str aws-auth-mac aws-auth-mac-encode
 sdb-auth-str sdb-auth-mac-encode)
  
(require/typed srfi/13
	       (string-trim-both (String -> String)))

(require
 (only-in (planet knozama/common:1/text/util)
	  weave-string-separator)
 (only-in (planet knozama/webkit:1/crypto/base64)
	  base64-encode)
 (only-in (planet knozama/webkit:1/crypto/hmac)
	  hmac-sha1 hmac-sha256)
 (only-in (planet knozama/webkit:1/web/uri)
	  url-encode-string)
 (only-in (planet knozama/webkit:1/web/uri/url/param)
	  params->query)
 (only-in "configuration.rkt"
	  sdb-host sdb-std-params)
 (only-in "credential.rkt"
	  Aws-Credential-access-key
	  current-aws-credential))

;; &Version=2009-04-15
;; &Timestamp=2010-01-25T15%3A01%3A28-07%3A00
;; &SignatureVersion=2
;; &SignatureMethod=HmacSHA256

(: param-sort ((Pair String String) (Pair String String) -> Boolean))
(define (param-sort p1 p2)
  (string<=? (car p1) (car p2)))

;; SimpleDB Auth String to sign
(: sdb-auth-str (String (Listof (Pair String String)) -> String))
(define (sdb-auth-str action params)
  (let ((sep "\n")
      (std-params (sdb-std-params (Aws-Credential-access-key (current-aws-credential)))))
    (let ((qstr (params->query (sort (append std-params params) param-sort))))
      (string-append (weave-string-separator sep  (list action sdb-host "" qstr))))))

(: sdb-auth-mac (String String -> String))
(define (sdb-auth-mac key str)
  (string-trim-both (base64-encode (hmac-sha256 (string->bytes/utf-8 key)
						(string->bytes/utf-8 str)))))

(: sdb-auth-mac-encode (String String -> String))
(define (sdb-auth-mac-encode key str)
  (url-encode-string (sdb-auth-mac key str) #f))
	
(: aws-auth-str (String String String String (Listof String) String -> String))
(define (aws-auth-str verb md5 mime expiration amz-headers resource)
  (let ((sep "\n"))
    (if (null? amz-headers)
       (weave-string-separator sep (list verb md5 mime expiration resource))
       (weave-string-separator sep (list verb md5 mime expiration 
				     (weave-string-separator sep amz-headers) resource)))))

(: aws-auth-mac (String String -> String))
(define (aws-auth-mac key str)
  (string-trim-both (base64-encode (hmac-sha1 (string->bytes/utf-8 key) 
					      (string->bytes/utf-8 str)))))

(: aws-auth-mac-encode (String String -> String))
(define (aws-auth-mac-encode key str)
  (url-encode-string (aws-auth-mac key str) #f))
