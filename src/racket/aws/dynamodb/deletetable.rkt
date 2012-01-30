#lang typed/racket/base

(require
 racket/pretty
 (only-in "action.rkt"
	  DELETE-TABLE)
 (only-in "dynamodb.rkt"
	  dynamodb))

(struct: DeleteTableResp ())

(: delete-table (String -> DeleteTableResp))
(define (delete-table name)
  (pretty-print (dynamodb DELETE-TABLE (format "{\"TableName\": ~s}" name)))
  (DeleteTableResp))


;; // This header is abbreviated. 
;; // For a sample of a complete header, see Sample Amazon DynamoDB JSON Request and Response.
;; POST / HTTP/1.1 
;; x-amz-target: DynamoDB_20111205.DeleteTable 
;; content-type: application/x-amz-json-1.0

;; {"TableName":"Table1"}
