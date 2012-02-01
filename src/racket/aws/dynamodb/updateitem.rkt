#lang typed/racket/base

(require
 racket/pretty
 (only-in (planet knozama/webkit:1/formats/tjson)
	  Json JsObject jsobject json->string)
 (only-in "action.rkt"
	  UPDATE-ITEM)
 (only-in "types.rkt"
	  ItemKey Exists Item ReturnValues)
 (only-in "invoke.rkt"
	  dynamodb)
 (only-in "request.rkt"
	  return-values-json itemkey-json))

(struct: UpdateItemResp ())

(: update-item-request (String ItemKey (U Exists Item) (Listof Item) ReturnValues -> String))
(define (update-item-request table item-key expected attrs return-values)
  (let ((req (jsobject `((TableName . , table)
			 (Key . ,(itemkey-json item-key))
			 (ReturnValues . ,(return-values-json return-values))))))
    (json->string req)))

(: update-item (String ItemKey (U Exists Item) (Listof Item) ReturnValues -> UpdateItemResp))
(define (update-item table item-key expected attrs return-values)
  (let ((req (update-item-request table item-key expected attrs return-values)))
    (let ((resp (dynamodb UPDATE-ITEM req)))
      (pretty-print resp)))
  (UpdateItemResp))

;; {"TableName":"Table1",
;;     "Key":
;;         {"HashKeyElement":{"S":"AttributeValue1"},
;;         "RangeKeyElement":{"N":"AttributeValue2"}},
;;     "AttributeUpdates":{"AttributeName3":{"Value":{"S":"AttributeValue3_New"},"Action":"PUT"}},
;;     "Expected":{"AttributeName3":{"Value":{"S":"AttributeValue3_Current"}}},
;;     "ReturnValues":"ReturnValuesConstant"
;; }
