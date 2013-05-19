#lang racket

(provide 
 lnfact
 sample-gaussian gaussian-lnpdf
 sample-real random-real
 random-integer sample-integer
 sample-dirichlet dirichlet-lnpdf dirichlet
 discrete-sampler sample-discrete discrete-pdf)

;; All this is temporary until I can move to Racket math/distributions library.
(define (sample thunk) (thunk))

(define (sample-integer) (random 100))

(define (sample-real) 0.5)

(define (sample-dirichlet n) 0.6)

(define (sample-discrete) 0.75)

(define dirichlet 0.4)

(define (dirichlet-lnpdf n v) 0.7)

(define (discrete-sampler) 0.1)

(define (discrete-pdf) 0.2)

(define (random-integer n) (random n))

;; Best guess this is random-real from SRFI-27
;; Take a shot for now.  RPR
(define (random-real)
  (/ (random 10000) 10000))

(define (sample-gaussian mean var)
  0.9)

(define (gaussian-lnpdf m v)
  0.95)

(define (lnfact n) (log n))
