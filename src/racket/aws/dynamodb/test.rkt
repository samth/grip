#lang typed/racket/base

(require
 (only-in "dynamodb.rkt"
	  create-table delete-table describe-table list-tables 
	  get-item put-item 
	  Item Key ItemKey KeyVal
	  ReturnValues Throughput))

(define table "product")

(define (create)
  (create-table table (Key "sku" 'String) #f (Throughput 3 5)))

(define (describe)
  (describe-table table))

(define (add)
  (put-item table (list (Item "color" "red" 'String)
			(Item "price" "1.99" 'Number)
			(Item "sku" "315515" 'String))
	    #f 'AllOld))

(define (add2)
  (put-item table (list (Item "color" "blue" 'String)
			(Item "price" "5.00" 'Number)
			(Item "sku" "123456" 'String))
	    #f 'AllOld))

(define (get)
  (get-item table (ItemKey (KeyVal "315515" 'String) #f) '("sku" "price") #f))
 
(define (delete)
  (delete-table table))

(define (dir)
  (list-tables #f 10))

;; '#hasheq((TableDescription
;;           .
;;           #hasheq((TableStatus . "CREATING")
;;                   (CreationDateTime . 1.3280218373939)
;;                   (ProvisionedThroughput
;;                    .
;;                    #hasheq((WriteCapacityUnits . 5) (ReadCapacityUnits . 3)))
;;                   (KeySchema
;;                    .
;;                    #hasheq((HashKeyElement
;;                             .
;;                             #hasheq((AttributeType . "S")
;;                                     (AttributeName . "upc")))))
;;                   (TableName . "product"))))


;; PUT

;; racket@aws/dynamodb/test> (add)
;; "GET\nsts.amazonaws.com\n/\nAWSAccessKeyId=0JQG3Q3MF57Z6FDP2MG2&Action=GetSessionToken&SignatureMethod=HmacSHA256&SignatureVersion=2&Timestamp=2012-01-31T10%3A04%3A05-0500&Version=2011-06-15"
;; "POST\n/\n\nhost:dynamodb.us-east-1.amazonaws.com\nx-amz-date:Tue, 31 Jan 2012 10:04:05 -0500\nx-amz-security-token:AQoDYXdzENj//////////wEasAHlCOiXWE9bm2fC7ZtTH9mRs1ALyLAYl/dBHAMkrtJYlOELgnAok895WkZ9AoHpe6maaTafRaw7xOkFkpbBkzluLmhcDJjVLZDkn7cn6Aaf3rB8WxK8LMheNTMPEGyYloGCg4LVl2RfivkSeXg+pScTnEntzJ2SX30hL75I0NFP82Ujssfkq4/uOS7hgiXn3FSto1rxPW5eoWGeKNIB9/Zs3FV5Plb+e++vOEyt7qY1NCDlhaD5BA==\nx-amz-target:DynamoDB_20111205.PutItem\n\n{\"ReturnValues\": \"ALL_OLD\", \"TableName\": \"product\", \"Item\": {\"color\": {\"S\": \"red\"}, \"price\": {\"N\": \"1.99\"}, \"upc\": {\"S\": \"315515\"}}}"
;; Sending payload of size132
;; '#hasheq((ConsumedCapacityUnits . 1.0))
;; - : (U DDBError PutItemResult)
;; (PutItemResult)
;; racket@aws/dynamodb/test> (add)
;; "GET\nsts.amazonaws.com\n/\nAWSAccessKeyId=0JQG3Q3MF57Z6FDP2MG2&Action=GetSessionToken&SignatureMethod=HmacSHA256&SignatureVersion=2&Timestamp=2012-01-31T10%3A04%3A36-0500&Version=2011-06-15"
;; "POST\n/\n\nhost:dynamodb.us-east-1.amazonaws.com\nx-amz-date:Tue, 31 Jan 2012 10:04:37 -0500\nx-amz-security-token:AQoDYXdzENj//////////wEasAGf0W+sXWImo+XlTTRy+N2yPxSu15HpOdT63V8k4XQ5J8c4Nsus8Tw4xSPw8OL42kwnOBTiSJ8z9u4IHg+oSHocyTiwnDOA4MtQqjOXFor3EXK4PedGrPWXI8cq70V1BdPfBvHamJj+R9SPT4zi5jo0T40lJEOA+mVC3dgyNpMbjLIctD18FS6ILf5BNseXpWT6ALsvd+n2D2V/Br8nkO+L3+szOxh7LLXS3wmCPFE7+CCFhqD5BA==\nx-amz-target:DynamoDB_20111205.PutItem\n\n{\"ReturnValues\": \"ALL_OLD\", \"TableName\": \"product\", \"Item\": {\"color\": {\"S\": \"red\"}, \"price\": {\"N\": \"1.99\"}, \"upc\": {\"S\": \"315515\"}}}"
;; Sending payload of size132
;; '#hasheq((Attributes
;;           .
;;           #hasheq((upc . #hasheq((S . "315515")))
;;                   (price . #hasheq((N . "1.99")))
;;                   (color . #hasheq((S . "red")))))
;;          (ConsumedCapacityUnits . 1.0))
;; - : (U DDBError PutItemResult)
;; (PutItemResult)


;; "{\"message\": \"Supplied AttributebValue is empty, must contain exactly one of the supported datatypes\", \"__type\": \"com.amazon.coral.validate#ValidationException\"}"
