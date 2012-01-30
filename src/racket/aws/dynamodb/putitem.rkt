#lang typed/racket/base

(require 
 racket/pretty
 (only-in (planet knozama/webkit:1/formats/tjson)
	  Json JsObject json->string jsobject attribute)
 (only-in "types.rkt"
	  DDBError
	  DDBType ddbtype-symbol))

;; POST / HTTP/1.1 
;; x-amz-target: DynamoDB_20111205.PutItem
;; content-type: application/x-amz-json-1.0

;; {"TableName":"Table1",
;; 	"Item":{
;; 		"AttributeName1":{"AttributeValue1":"S"},
;; 		"AttributeName2":{"AttributeValue2":"N"},
;; 	},
;; 	"Expect":{"AttributeName3":{"Value": {"S":"AttributeValue"},{"Exists":Boolean}},
;; 	"ReturnValues":"ReturnValuesConstant"}

(define-type ReturnValues (U 'None 'AllOld))

(struct: Item ([name : String] [value : String] [type : DDBType]) #:transparent)

(struct: Exists ([name : String] [exists : Boolean]) #:transparent)

(struct: PutItemResult () #:transparent)

(: items-obj (Item -> (Pair Symbol JsObject)))
(define (items-obj item)
  (cons (string->symbol (Item-name item))
	(jsobject `((,(ddbtype-symbol (Item-type item)) . ,(Item-value item))))))

(: item-request ((Listof Item) -> JsObject))
(define (item-request items)
  (jsobject (map items-obj items)))

(: expected-json (Item -> JsObject))
(define (expected-json expected)
  (jsobject (list (items-obj expected))))

(: exists-json (Exists -> JsObject))
(define (exists-json exists)
  (jsobject `((,(string->symbol (Exists-name exists)) . ,(jsobject `((Exists . ,(Exists-exists exists))))))))

(: return-values-request (ReturnValues -> String))
(define (return-values-request rtnval)
  (case rtnval
    ((None) "NONE")
    ((AllOld) "ALL_OLD")))

(: put-item-request (String (Listof Item) (Option (U Exists Item)) ReturnValues -> JsObject))
(define (put-item-request name items expected return-values)
  (let: ((req : JsObject (make-hasheq `((TableName . ,name)
					(Item . ,(item-request items))
					(ReturnValues . ,(return-values-request return-values))))))
    (when expected
      (cond 
       ((Item? expected) (attribute req 'Expect (expected-json expected)))
       ((Exists? expected)   (attribute req 'Expect (exists-json expected)))))
    req))

(: put-item (String (Listof Item) (Option (U Exists Item)) ReturnValues -> (U DDBError PutItemResult)))
(define (put-item name items expected return-values)
  (pretty-print (dynamodb PUT-ITEM (put-item-request name items expected return-values)))
  (PutItemResult))

  
;; (define (test)
;;   (put-item "Ray" (list (Item "color" "red" 'String)
;; 			(Item "age" "50" 'Number))
;; 	    (Item "Name" "ray" 'String) 'None)
  
;;   (put-item "Ray" (list (Item "color" "red" 'String)
;; 			(Item "age" "50" 'Number)) #f 'None))

;; (define (etest)
;;   (pretty-print (json->string (expected-json (Item "color" "red" 'String)))))

;; (define (xtest)
;;   (pretty-print (json->string (exists-json (Exists "color" #t)))))
