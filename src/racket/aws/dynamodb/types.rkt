#lang typed/racket/base

(provide
 DDBError DDBError? DDBError-code
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
