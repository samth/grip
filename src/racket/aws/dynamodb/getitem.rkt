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

(require
 racket/pretty
 (only-in (planet knozama/webkit:1/formats/tjson)
 	  Json JsObject JsObject? json->string string->json jsobject attribute)
 (only-in (planet knozama/common:1/std/opt)
	  opt-orelse)
 (only-in "types.rkt"
	  ddbtype-symbol DDBType
	  DDBError DDBError? DDBError-code
	  Item
	  Key Key? Key-name Key-type
	  KeyVal KeyVal? KeyVal-value)
 (only-in "action.rkt"
	  GET-ITEM)
 (only-in "invoke.rkt"
	  dynamodb))
 
(struct: GetItemResp ([items : (Listof Item)] [consumed : Float]) #:transparent)

(struct: ItemKey ([hashkey : KeyVal]
		  [rangekey : (Option KeyVal)]) #:transparent)

(: keyvalue-json (KeyVal -> JsObject))
(define (keyvalue-json keyval)
  (jsobject `((,(string->symbol (Key-name keyval)) . ,(jsobject `((,(ddbtype-symbol (Key-type keyval)) . ,(KeyVal-value keyval))))))))

(: itemkey-json (ItemKey -> JsObject))
(define (itemkey-json item-key)
  (let ((jsobj (jsobject `((HashKeyElement . ,(keyvalue-json (ItemKey-hashkey item-key)))))))
    (let ((rnge-key (ItemKey-rangekey item-key)))
      (when rnge-key 
	(attribute jsobj 'RangeKeyElement (keyvalue-json rnge-key))))
    jsobj))

(: get-item-request (String ItemKey (Listof String) Boolean -> String))
(define (get-item-request name key attrs consistent?)
  (let ((req (jsobject `((TableName . ,name)
			 (Key . ,(itemkey-json key))
			 (ConsistentRead . ,(if consistent? "true" "false"))))))
    (json->string req)))

(: get-item  (String ItemKey (Listof String) Boolean -> GetItemResp))
(define (get-item name item-key attrs consistent?)
  (let ((req (get-item-request name item-key attrs consistent?)))
    (let ((resp (dynamodb GET-ITEM req)))
      (if (JsObject? resp)
	  (parse-get-item-resp resp)
	  (error "Invalid response ~a" resp)))))

;;; Parse Response

(: parse-fail (Json -> Nothing))
(define (parse-fail json)
  (error "Invalid response: " (json->string json)))

(: parse-get-item-resp (JsObject -> GetItemResp))
(define (parse-get-item-resp resp)

  (: parse-item-value (Symbol Json -> String))
  (define (parse-item-value type json)    
    (if (JsObject? json)
	(let ((value (hash-has-key? json type)))
	  (if (string? value)
	      value
	      (parse-fail json)))
	(parse-fail json)))	  

  (: parse-item (String JsObject -> Item))
  (define (parse-item name json)
    (cond 
     ((hash-has-key? json 'S) (Item (parse-item-value 'S json) name 'String))
     ((hash-has-key? json 'N) (Item (parse-item-value 'N json) name 'Number))
     (else (error "Invalid response ~s" json))))

  (: parse-items (JsObject -> (Listof Item)))
  (define (parse-items attrs)
    (let: loop : (Listof Item) ((attrs : (Listof (Pair Symbol JsObject)) (hash->list attrs)) 
				(items : (Listof Item) '()))
      (if (null? attrs)
	  items
	  (let* ((jitem (car items))
		 (name (car jitem))
		 (type-value (cdr jitem)))
	    (loop (cdr attrs) (cons (parse-item name type-value) items))))))

  (let ((jsresp (string->json resp)))
    (let ((items (parse-items (hash-ref jsresp 'Item)))
	  (consumed ((inst opt-orelse Number) (string->number (hash-ref jsresp 'ConsumedCapacityUnits)) 0.0)))      
      (GetItemResp items consumed))))

;; (define (test)
;;   (get-item "raytbl" (ItemKey (KeyVal "sku" 'String "315515") 
;; 			      (KeyVal "date" 'String "20120130"))
;; 	    (list "price") #f))
  
;; POST / HTTP/1.1 
;; x-amz-target: DynamoDB_20111205.GetItem
;; content-type: application/x-amz-json-1.0 

;; {"TableName":"Table1",
;;  	"Key": 
;; 		{"HashKeyElement": {"S":"AttributeValue1"},
;; 		"RangeKeyElement": {"N":"AttributeValue2"} 
;; 	},
;; 	"AttributesToGet":["AttributeName3","AttributeName4"],
;; 	"ConsistentRead":Boolean
;; }


;; HTTP/1.1 200 
;; x-amzn-RequestId: 8966d095-71e9-11e0-a498-71d736f27375 
;; content-type: application/x-amz-json-1.0
;; content-length: 144

;; {"Item":{
;; 	"AttributeName3":{"S":"AttributeValue3"},
;; 	"AttributeName4":{"N":"AttributeValue4"}
;; 	},
;; "ConsumedCapacityUnits": 0.5
;; }
