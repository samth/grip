#lang typed/racket/base

(require 
 (only-in "types.rkt"
	  DDBError DDBError? DDBError-code))

(struct: Throughput ([read : Natural] 
		     [write : Natural]) #:transparent)

(struct: Key ([name : String]
	      [type : String]) #:transparent)

(struct: Schema ([name : String]
		 [hash-key : Key]
		 [range-key : (Option Key)]
		 [throughput : Throughput]) #:transparent)

(: create-table (String Schema Throughput -> (U DDBError CreateTableResp)))
(define (create-table name schema throughput)
  )

