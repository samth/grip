#lang typed/racket/base

(provide 
 (struct-out Field)
 (struct-out Layout))

(struct: Field ([name : Symbol]
                [type : Symbol]
                [offset : Natural]
                [length : Natural]) #:transparent)

(struct: Layout ([name : Symbol]
                 [fields : (Listof Field)]) #:transparent)