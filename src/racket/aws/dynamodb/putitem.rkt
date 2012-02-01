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
  put-item
  PutItemResult PutItemResult?)

(require 
 racket/pretty
 (only-in (planet knozama/webkit:1/formats/tjson)
	  Json JsObject json->string jsobject attribute)
 (only-in "action.rkt"
	  PUT-ITEM)
 (only-in "types.rkt"
	  ReturnValues
	  Exists Exists? Exists-name Exists-exists
	  Item Item? Item-name Item-value Item-type
	  DDBType ddbtype-symbol)
 (only-in "invoke.rkt"
	  dynamodb)
 (only-in "request.rkt"
	  items-obj
	  expected/exists-json
	  return-values-json))

;; {"TableName":"Table1",
;; 	"Item":{
;; 		"AttributeName1":{"AttributeValue1":"S"},
;; 		"AttributeName2":{"AttributeValue2":"N"},
;; 	},
;; 	"Expect":{"AttributeName3":{"Value": {"S":"AttributeValue"},{"Exists":Boolean}},
;; 	"ReturnValues":"ReturnValuesConstant"}

(struct: PutItemResult () #:transparent)

(: item-request ((Listof Item) -> JsObject))
(define (item-request items)
  (jsobject (map items-obj items)))

(: put-item-request (String (Listof Item) (Option (U Exists Item)) ReturnValues -> String))
(define (put-item-request name items expected return-values)
  (let: ((req : JsObject (jsobject `((TableName . ,name)
				     (Item . ,(item-request items))
				     (ReturnValues . ,(return-values-json return-values))))))
    (when expected
      (attribute req 'Expect (expected/exists-json expected)))
    (json->string req)))

(: put-item (String (Listof Item) (Option (U Exists Item)) ReturnValues -> PutItemResult))
(define (put-item name items expected return-values)
  (pretty-print (dynamodb PUT-ITEM (put-item-request name items expected return-values)))
  (PutItemResult))

;; '#hasheq((Attributes
;;           .
;;           #hasheq((upc . #hasheq((S . "315515")))
;;                   (price . #hasheq((N . "1.99")))
;;                   (color . #hasheq((S . "red")))))
;;          (ConsumedCapacityUnits . 1.0))

  
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
