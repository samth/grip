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

(require
 racket/pretty
 (only-in (planet knozama/webkit:1/web/uri)
	  Uri Uri-query make-uri parse-uri
	  url-encode-string uri->string)
 (only-in (planet knozama/webkit:1/web/http/http11)
	  HTTPConnection-in http-successful? http-close-connection http-invoke)
 (only-in (planet knozama/webkit:1/web/uri/url/param)
	  Params)
 (only-in (planet knozama/webkit:1/web/http/header)
          make-header-string)
 (only-in (planet knozama/webkit:1/formats/json)
	  json->sjon)
 (only-in "../auth.rkt"
	  ddb-request-signature)
 (only-in "config.rkt"
	  ddb-host)
 (only-in (planet knozama/aws:1/credential)
	  Aws-Credential-secret-key Aws-Credential-access-key current-aws-credential))

(define-type Json String)

(define version "DynamoDB_20111205")
 
(define LIST-TABLES (string-append version "ListTables"))

(struct: DynamoDBFailure () #:transparent)

(struct: ListTablesResp ([names : (Listof String)]
			 [last : String]) #:transparent)

(: request-headers (Listof String))
(define request-headers  
  (list 
   ;; (make-header-string "User-Agent" "Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/535.11 (KHTML, like Gecko) Chrome/17.0.963.2 Safari/535.11")
   (make-header-string "User-Agent" "Googlebot/2.1 (+http://www.google.com/bot.html)")
   (make-header-string "Accept" "text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8")
   (make-header-string "Accept-Charset" "ISO-8859-1,utf-8;q=0.7,*;q=0.3")
   (make-header-string "Accept-Encoding" "gzip")
   (make-header-string "Accept-Language" "en-US,en;q=0.8")
   (make-header-string "Cache-Control" "max-age=0")
   (make-header-string "Content-Type" "application/x-amz-json-1.0")
   (make-header-string "Connection" "Close")))

(: auth-headers Params)
(define auth-headers
  (list 
   (cons "host" ddb-host)
   (cons "x-amz-target" "DynamoDB_20111205.ListTables")))

(: list-tables ((Option String) (Option Integer) -> ListTablesResp))
(define (list-tables startat limit)
  (ListTablesResp '() ""))

(: dynamodb-invoke (Uri (Listof String) Json -> (U DynamoDBFailure Json)))
(define (dynamodb-invoke url headers payload)
  (with-handlers ([exn:fail?
		   (lambda (ex) (DynamoDBFailure))])
    (let ((conn (http-invoke 'POST url headers (string->bytes/utf-8 payload))))
      (pretty-print conn)
      "done")))

(: sign-request (Params Json -> String))
(define (sign-request params body)
  (string-append "Signature=" (ddb-request-signature (Aws-Credential-secret-key (current-aws-credential)) params body)))

(: test (-> Void))
(define (test)
  (let ((url (make-uri "http" #f ddb-host 80 "/" #f #f))
      (body "{\"ExclusiveStartTableName\":\"\",\"Limit\":99}"))
    (pretty-print (uri->string url))
    (let* ((auth (make-header-string "x-amzn-authorization"
				   (string-append "AWS3 AWSAccessKeyId=" 
						  (Aws-Credential-access-key (current-aws-credential))
						  ",Algorithm=HmacSHA256," (sign-request auth-headers body))))
	 (headers (cons auth request-headers)))
      (pretty-print headers)
      (let (respons(dynamodb-invoke url headers body)
      (void))))
    

;; x-amzn-authorization: AWS3 AWSAccessKeyId=*Current Access Key*,Algorithm=HmacSHA256,SignedHeaders=Host;x-amz-date;x-amz-target;x-amz-security-token,Signature=*Signature Value*=
;; Date: Mon, 31 Oct 2011 17:49:52 GMT

;; POST / HTTP/1.1
;; Host: dynamodb.us-east-1.amazonaws.com
;; x-amz-date: Mon, 16 Jan 2012 17:50:52 GMT
;; x-amzn-authorization: AWS3 AWSAccessKeyId=*Current Access Key*,Algorithm=HmacSHA256,SignedHeaders=Host;x-amz-date;x-amz-target;x-amz-security-token,Signature=*Signature Value*=
;; Date: Mon, 31 Oct 2011 17:49:52 GMT
;; x-amz-target: DynamoDB_20111205.GetItem
;; x-amz-security-token: *Token Value*
;; Content-Type: application/x-amz-json-1.0
;; Content-Length: 135
;; Connection: Keep-Alive
;; User-Agent: aws-sdk-java/1.2.10 Windows_7/6.1 Java_HotSpot(TM)_64-Bit_Server_VM/20.2-b06

;; {"TableName":"my-table",
;;     "Keys":
;;         [{"HashKeyElement":{"S":"Bill & Ted's Excellent Adventure"},
;;         "RangeKeyElement":{"S":1989}}]
;; }
