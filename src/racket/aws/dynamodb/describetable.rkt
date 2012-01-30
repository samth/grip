#lang typed/racket/base

(require
 racket/pretty
 (only-in "action.rkt"
	  DESCRIBE-TABLE)
 (only-in "dynamodb.rkt"
	  dynamodb))

(struct: DescribeTableResp ())

(: describe-table (String -> DescribeTableResp))
(define (describe-table name)
  (pretty-print (dynamodb DESCRIBE-TABLE (format "{\"TableName\": ~s}" name)))
  (DescribeTableResp))


;; // This header is abbreviated. 
;; // For a sample of a complete header, see Sample Amazon DynamoDB JSON Request and Response.
;; POST / HTTP/1.1 
;; x-amz-target: DynamoDB_20111205.DescribeTable
;; content-type: application/x-amz-json-1.0

;; {"TableName":"Table1"}
