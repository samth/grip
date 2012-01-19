#lang typed/racket/base

(require
 (only-in (planet knozama/webkit:1/web/uri)
	  Uri Uri-query make-uri parse-uri
	  url-encode-string uri->string))

(define-type Json String)

(define version "DynamoDB_20111205")
 
(define LIST-TABLES (string-append version "ListTables"))

(struct: DynamoDBFailure () #:transparent)

(struct: ListTablesResp ([names : (Listof String)]
			 [last : String]) #:transparent)

(: list-tables ((Option String) (Option Integer) -> ListTablesResp))
(define (list-tables startat limit)
  (ListTablesResp '() ""))

(: dynamodb-invoke (Uri Json -> (U DynamoDBFailure Json)))
(define (dynamodb-invoke uri payload)
  (with-handlers ([exn:fail?
		   (lambda (ex) (DynamoDBFailure))])
    (let ((conn (http-invoke 'POST url headers payload)))
      (pretty-print conn)
      "done")))
		  

