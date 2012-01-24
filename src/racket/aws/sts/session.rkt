#lang typed/racket/base

(provide 
 (all-defined-out))

(struct: STSError ())

(struct: SessionCredential  ([token : String] 
			     [access-key : String] 
			     [secret-key : String]
			     [expiration : String]))


