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

(module awsauth mzscheme
  
  (require "awsmisc.scm"
           (lib "md5.ss")
           ;; (lib "head.ss" "net")
           (only (lib "13.ss" "srfi") string-trim-both)
           (lib "base64.ss" "net")
           (lib "uri-codec.ss" "net")
           (planet "hmac-sha1.ss" ("jaymccarthy" "hmac-sha1.plt" 1 1)))
  
  (provide md5-file 
           aws-s3-auth-str
           aws-s3-auth-mac
           aws-s3-auth-mac-encode)
  
  (define (md5-file fname)
    (let ((inf (open-input-file fname 'binary)))
      (let ((hash (md5 inf)))
        (close-input-port inf)
        hash)))    
  
  (define (aws-s3-auth-str verb md5 mime expiration amz-headers resource)
    (let ((sep "\n"))
      (if (null? amz-headers)
          (weave (list verb md5 mime expiration resource) sep)
          (weave (list verb md5 mime expiration (weave amz-headers sep) resource) sep))))
  
  (define (aws-s3-auth-mac key str)
    (string-trim-both (bytes->string/utf-8 (base64-encode (HMAC-SHA1 (string->bytes/utf-8 key) (string->bytes/utf-8 str))))))
  
  (define (aws-s3-auth-mac-encode key str)
    (uri-encode (aws-s3-auth-mac key str)))
  
  )