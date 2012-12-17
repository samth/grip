#lang typed/racket/base

(define-type MRCmd (U 'Map 'Reduce))

(struct: Task ([cmd : MRCmd]) #:prefab)