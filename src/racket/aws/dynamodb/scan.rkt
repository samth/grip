#lang typed/racket/base

(require 
 racket/pretty
 (only-in (planet knozama/webkit:1/formats/tjson)
	  Json JsObject JsObject? JsObject-empty
	  json->string jsobject attribute)
 (only-in "action.rkt"
	  SCAN)
 (only-in "invoke.rkt"
	  dynamodb)
 (only-in "request.rkt"
	  itemkey-json)
 (only-in "types.rkt"
	  ddbtype-symbol
	  Operator operator->string 
	  ItemVal ItemVal-type ItemVal-value
	  ItemKey KeyVal))

(struct: Filter ([item : String]
		 [values : (Listof ItemVal)]
		 [operator : Operator]) #:transparent)

(struct: ScanResponse () #:transparent)

(: item-val-jsobject (ItemVal -> JsObject))
(define (item-val-jsobject item-val)
  (jsobject `((,(ddbtype-symbol (ItemVal-type item-val)) . ,(ItemVal-value item-val)))))

(: scan-filter-values ((Listof ItemVal) -> (Listof JsObject)))
(define (scan-filter-values item-vals)  
  (map item-val-jsobject item-vals))

(: scan-filter (Filter -> (Pair Symbol Json)))
(define (scan-filter filter)
  (let ((condition (jsobject `((AttributeValueList . ,(scan-filter-values (Filter-values filter)))
			       (ComparisonOperator . ,(operator->string (Filter-operator filter)))))))
    `(,(string->symbol (Filter-item filter)) . ,condition)))

(: scan-request (String (Listof String) Exact-Positive-Integer Boolean (Listof Filter) (Option ItemKey) -> String))
(define (scan-request table attrs limit count? filters exclusive-start-key)
  (let: ((req-attrs : (Listof (Pair Symbol Json))
		    (let ((base-attrs
			   `((TableName . ,table)
			     (AttributesToGet . ,attrs)
			     (ScanFilter . ,(jsobject (map scan-filter filters)))
			     (Count . ,count?)
			     (Limit . ,limit))))
		      (if exclusive-start-key
			  (cons `(ExclusiveStartKey . ,(itemkey-json exclusive-start-key)) base-attrs)
			  base-attrs))))
    (let: ((req : JsObject (jsobject req-attrs)))
      (when (null? attrs)
	(hash-remove! req 'AttributesToGet))
    (pretty-print (json->string req))
    (json->string req))))
  
(: scan (String (Listof String) Exact-Positive-Integer Boolean (Listof Filter) (Option ItemKey) -> ScanResponse))
(define (scan table attrs limit count? filters exclusive-start-key)
  (pretty-print (dynamodb SCAN (scan-request table attrs limit count? filters exclusive-start-key)))
  (ScanResponse))

(: test (-> ScanResponse))
(define (test)
  (scan "product" '() 100 #f 
	(list (Filter "recycled" '() 'NULL))
	#f))

;; {"TableName":"comp5",
;; 	"ScanFilter":
;; 		{"time":
;; 			{"AttributeValueList":[{"N":"400"}],
;; 			"ComparisonOperator":"GT"}
;; 	}
;; }

(: test2 (-> String))
(define (test2)
  (scan-request "product" '("sku" "purveyor") 100 #f 
		(list (Filter "recycled" (list (ItemVal "Yes" 'String)) 'EQ))
		#f))
