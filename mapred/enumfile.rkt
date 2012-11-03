#lang typed/racket/base

(require 
 (only-in "rdd/types.rkt"
          Block Block-loc)
 (only-in "rdd/rdd.rkt"
          RDDFile RDDFile-blocks))

(: data-input-size (RDDFile -> Integer))
(define (data-input-size brdd)
  (foldl (λ: ((block : Block) (sz : Integer))
           (+ sz (file-size (Block-path block))))
         0
         (BaseRDD-blocks brdd)))
