#lang racket

(provide 
 (all-defined-out)
 (all-from-out rnrs/io/ports-6))

(require rnrs/io/ports-6)

;; (: transcoder? (Any -> Boolean))
(define (transcoder? coder)
  (eq? (vector-ref (struct->vector coder) 0) 'struct:transcoder))

;; (: codec? (Any -> Boolean))
(define (codec? codec)
  (eq? (vector-ref (struct->vector codec) 0) 'struct:codec))
