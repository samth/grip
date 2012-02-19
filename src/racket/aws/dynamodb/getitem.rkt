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
 get-item GetItemResp GetItemResp-items GetItemResp-consumed)

(require
 racket/pretty
 (only-in (planet knozama/webkit:1/formats/tjson)
	  JsObject-empty
 	  Json JsObject JsObject? json->string string->json jsobject attribute)
 (only-in (planet knozama/common:1/std/opt)
	  opt-orelse)
 (only-in "types.rkt"
	  ddbtype-symbol DDBType
	  Item
	  Key Key? Key-name Key-type
	  KeyVal KeyVal? KeyVal-value KeyVal-type
	  ItemKey)
 (only-in "action.rkt"
	  GET-ITEM)
 (only-in "invoke.rkt"
	  dynamodb)
 (only-in "request.rkt"
	  itemkey-json))	  
 
(struct: GetItemResp ([items : (HashTable String Item)] [consumed : Float]) #:transparent)

(: get-item-request (String ItemKey (Listof String) Boolean -> String))
(define (get-item-request table key attrs consistent?)
  (let ((req (jsobject `((TableName . ,table)
			 (Key . ,(itemkey-json key))
			 (AttributesToGet . ,attrs)
			 (ConsistentRead . ,(if consistent? "true" "false"))))))
    (json->string req)))

(: get-item  (String ItemKey (Listof String) Boolean -> GetItemResp))
(define (get-item table item-key attrs consistent?)
  (let ((req (get-item-request table item-key attrs consistent?)))
    (let ((resp (dynamodb GET-ITEM req)))
      (if (JsObject? resp)
	  (parse-get-item-resp resp)
	  (error "Invalid response ~a" resp)))))

;;; Parse Response

(: parse-get-item-resp (JsObject -> GetItemResp))
(define (parse-get-item-resp resp)

  (: parse-fail (Json -> Nothing))
  (define (parse-fail json)
    (error "Invalid response: " (json->string json)))

  (: parse-item-value (Symbol Json -> String))
  (define (parse-item-value type json)    
    (if (JsObject? json)
	(let ((value (hash-ref json type)))
	  (if (string? value)
	      value
	      (parse-fail json)))
	(parse-fail json)))	  

  (: parse-item (String JsObject -> Item))
  (define (parse-item name json)
    (cond 
     ((hash-has-key? json 'S) (Item name (parse-item-value 'S json) 'String))
     ((hash-has-key? json 'N) (Item name (parse-item-value 'N json) 'Number))	
     (else (parse-fail json))))

  (: parse-items (JsObject -> (HashTable String Item)))
  (define (parse-items jattrs)
    (let: ((items : (HashTable String Item) (make-hash)))
      (let: loop : (HashTable String Item) 
	    ((attrs : (Listof (Pair Symbol Json)) ((inst hash->list Symbol Json) jattrs)))
	    (if (null? attrs)
		items
		(let* ((jitem (car attrs))
		       (name (symbol->string (car jitem)))
		       (type-value (cdr jitem)))
		  (if (JsObject? type-value)
		      (begin
			(hash-set! items name (parse-item name type-value))
			(loop (cdr attrs)))
		      (parse-fail jattrs)))))))

  (pretty-print (json->string resp))
  (if (hash-has-key? resp 'ConsumedCapacityUnits)
      (let ((jconsumed (hash-ref resp 'ConsumedCapacityUnits)))
	(let ((items (hash-ref resp 'Item (lambda: () JsObject-empty))))
	  (if (and (JsObject? items)
		   (flonum? jconsumed))
	      (GetItemResp (parse-items items) jconsumed)
	      (parse-fail resp))))
      (parse-fail resp)))

