#lang typed/racket/base

(provide create-table
	 Key Key? Key-name Key-type
	 Throughput Throughput? Throughput-read Throughput-write)

(require 
 racket/pretty
 (only-in (planet knozama/webkit:1/formats/tjson)
	  Json JsObject json->string)
 (only-in "types.rkt"
	  DDBType ddbtype-code)
 (only-in "action.rkt"
	  CREATE-TABLE)
 (only-in "types.rkt"
	  DDBError DDBError? DDBError-code)
 (only-in "invoke.rkt"
	  dynamodb))

(struct: Throughput ([read : Natural] 
		     [write : Natural]) #:transparent)

(struct: Key ([name : String]
	      [type : DDBType]) #:transparent)

(struct: CreateTableResp ())

(: create-request (String Key (Option Key) Throughput -> String))
(define (create-request name hash-key range-key throughput)

  (: keys-json (Key (Option Key) -> JsObject))
  (define (keys-json hash-key range-key)
    (: key-json (Key -> JsObject))
    (define (key-json key)
      (make-hasheq `((AttributeName . ,(Key-name key))
		     (AttributeType . ,(ddbtype-code (Key-type key))))))

    (let: ((keys : JsObject (make-hasheq)))
      (hash-set! keys 'HashKeyElement (key-json hash-key))
      (when range-key
	(hash-set! keys 'RangeKeyElement (key-json hash-key)))
      keys))

  (: throughput-json (Throughput -> JsObject))
  (define (throughput-json throughput)
    (make-hasheq `((ReadCapacityUnits . ,(Throughput-read throughput))
		   (WriteCapacityUnits . ,(Throughput-write throughput)))))

  (json->string (make-hasheq `((TableName . ,name)
			       (KeySchema . ,(keys-json hash-key range-key))
			       (ProvisionedThroughput . ,(throughput-json throughput))))))

(: create-table (String Key (Option Key) Throughput -> (U DDBError CreateTableResp)))
(define (create-table name hash-key range-key throughput) 
  (pretty-print (dynamodb CREATE-TABLE (create-request name hash-key range-key throughput)))
  (DDBError 'Test))


(define (test)  
  (create-table "ray" (Key "sku" 'String) #f (Throughput 3 5)))

;; POST / HTTP/1.1 
;; x-amz-target: DynamoDB_20111205.CreateTable 
;; content-type: application/x-amz-json-1.0 

;; {"TableName":"Table1",
;;     "KeySchema":
;;         {"HashKeyElement":{"AttributeName":"AttributeName1","AttributeType":"S"},
;;         "RangeKeyElement":{"AttributeName":"AttributeName2","AttributeType":"N"}},
;;     "ProvisionedThroughput":{"ReadCapacityUnits":5,"WriteCapacityUnits":10}
;; }
