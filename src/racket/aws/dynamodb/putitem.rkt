;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Knozama's Amazon API Library
;; Copyright (C) 2012  Raymond Paul Racine
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
  put-item ReturnValues
  Exists Exists?
  PutItemResult PutItemResult?)

(require 
 racket/pretty
 (only-in (planet knozama/webkit:1/formats/tjson)
	  Json JsObject json->string jsobject attribute)
 (only-in "action.rkt"
	  PUT-ITEM)
 (only-in "types.rkt"
	  Item Item? Item-name Item-value Item-type
	  DDBError
	  DDBType ddbtype-symbol)
 (only-in "invoke.rkt"
	  dynamodb))

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

(: put-item-request (String (Listof Item) (Option (U Exists Item)) ReturnValues -> String))
(define (put-item-request name items expected return-values)
  (let: ((req : JsObject (jsobject `((TableName . ,name)
				     (Item . ,(item-request items))
				     (ReturnValues . ,(return-values-request return-values))))))
    (when expected
      (cond 
       ((Item? expected) (attribute req 'Expect (expected-json expected)))
       ((Exists? expected)   (attribute req 'Expect (exists-json expected)))))
    (json->string req)))

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
