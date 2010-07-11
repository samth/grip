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

(module s3-test mzscheme
  
  (require "s3types.scm"
           "awsauth.scm"
           "awscredentials.scm"
           (planet "text-ui.ss" ("schematics" "schemeunit.plt" 2 8))
           (planet "test.ss" ("schematics" "schemeunit.plt" 2 8))
           (planet "util.ss" ("schematics" "schemeunit.plt" 2)))
  
  (require/expose "s3.scm" (authorization-header 
                            list-buckets
                            list-bucket-objects
                            put-object 
                            put-file-object))
  
  (define account-props "/home/ray/awsaccount.txt") 
  (define credentials (load-credentials account-props))  
  
  (define example-credentials (make-aws-credentials 
                               "---"
                               "44CF9590006BF252F707"
                               "OtxrzxIsfpFjA7SwPzILwy8Bw21TLhquhboDYROV"))
  
  (define test-file "/code/editio_princeps/books/oz.lrf")
  (define hello-file "/code/editio_princeps/books/hello.txt")
  (define large-file "/code/editio_princeps/books/love.lrf")
  
  (define (key fname)
    (let-values (((dir file must-dir) (split-path fname)))
      (let ((fname (path->string file)))
        (make-s3-key (list "a" "b" "c" fname)))))
  
  (define test-bucket (make-s3-bucket "bravaistest"))
  (define test-key (key hello-file))  
  (define test-resource (make-s3-resource test-bucket test-key))
  
  (define example-resource (make-s3-resource (make-s3-bucket "bravaistest")
                                             (make-s3-key '("quotes" "nelson"))))
  
  ; PUT /quotes/nelson HTTP/1.0
  ; Content-Md5: c8fdb181845a4ca6b8fec737b3581d76
  ; Content-Type: text/html
  ; Date: Thu, 17 Nov 2005 18:49:58 GMT
  ; X-Amz-Meta-Author: foo@bar.com
  ; X-Amz-Magic: abracadabra 
  
  (define aws-example-1
    (test-suite
     "AWS Document Signing Example #1"       
     (let ((aws-str
            "PUT\nc8fdb181845a4ca6b8fec737b3581d76\ntext/html\nThu, 17 Nov 2005 18:49:58 GMT\nx-amz-magic:abracadabra\nx-amz-meta-author:foo@bar.com\n/quotes/nelson")
           (aws-hdr "Authorization: AWS 44CF9590006BF252F707:jZNOcbfWmD/A/f3hSvVzXZjM2HU="))
       
       (test-equal? "1st Example AWS Str" 
                    (aws-s3-auth-str "PUT" "c8fdb181845a4ca6b8fec737b3581d76" "text/html" "Thu, 17 Nov 2005 18:49:58 GMT"
                                     (list "x-amz-magic:abracadabra" "x-amz-meta-author:foo@bar.com") 
                                     "/quotes/nelson") 
                    aws-str)
       
       (test-equal? "1st Example Auth Header" 
                    (authorization-header example-credentials aws-str) 
                    aws-hdr))))
  
  (test/text-ui 
   (test-suite "S3 Document Signing Examples" 
               aws-example-1))
  
  
;;  (put-object credentials (string->bytes/utf-8 "Hello") (make-s3-resource test-bucket test-key))
  
  ;(create-bucket credentials test-bucket)  
  (put-file-object credentials (string->path hello-file) (make-s3-resource test-bucket test-key))  
  ;(list-bucket-objects credentials test-bucket (make-s3-key '("a" "b" "c")) "" 10)
  
  ; (list-buckets credentials)    
  
  
  ;(delete-object credentials test-resource)
  ;(delete-bucket credentials test-bucket)
  
  ;  (list-bucket-objects credentials test-bucket (make-s3-key '("")) "" 10)
  ;  
  ;  (delete-bucket credentials test-bucket)
  ;  (list-buckets credentials)
  
  )




