#lang typed/racket/base

(provide
 DDBError DDBError? DDBError-code
 Key Key? Key-name Key-type
 KeyVal KeyVal? KeyVal-value
 Item Item? Item-name Item-value Item-type
 ddbtype-code ddbtype-symbol DDBType)

(struct: DDBError ([code : Symbol]))

(define-type DDBType (U 'String 'Number))

(: ddbtype-code (DDBType -> String))
(define (ddbtype-code type)
  (case type
    ((String) "S")
    ((Number) "N")))

(: ddbtype-symbol (DDBType -> Symbol))
(define (ddbtype-symbol type)
  (case type
    ((String) 'S)
    ((Number) 'N)))

(struct: Key ([name : String]
	      [type : DDBType]) #:transparent)

(struct: KeyVal Key ([value : String]) #:transparent)

(struct: Item ([name : String] [value : String] [type : DDBType]) #:transparent)
