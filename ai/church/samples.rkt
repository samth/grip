#lang racket

(require 
 (only-in "mcmc-preamble.rkt" mh-query))

(define church-samples
  (mh-query
   100
   250
   (lambda ()
     (letrec ((church-mean (church-gaussian 0 1))
              (church-var (church-abs (church-gaussian 0 1)))
              (church-sample-datum (λ ()
                                     (church-gaussian church-mean church-var))))))))