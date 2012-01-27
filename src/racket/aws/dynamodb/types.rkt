#lang typed/racket/base

(provide
 DDBError DDBError? DDBError-code)

(struct: DDBError ([code : Symbol]))
