#lang typed/racket/base

(provide
 CategoricalSeriesBuilder
 CategoricalSeriesBuilder?
 mkCategoricalSeriesBuilder
 append-CategoricalSeriesBuilder
 complete-CategoricalSeriesBuilder)

(require
 (only-in "../frame/categorical-series.rkt"
          CategoricalSeries))

(struct: CategoricalSeriesBuilder ([index : Index]
                                   [ord : Index]
                                   [data : (Vectorof Index)]
                                   [nominals : (HashTable Symbol Index)]) #:mutable #:transparent)

;; Make a SeriesBuilder for a CategoricalSeries
(: mkCategoricalSeriesBuilder (-> CategoricalSeriesBuilder))
(define (mkCategoricalSeriesBuilder)
  (define default-size 32)
  (CategoricalSeriesBuilder 0 0 (make-vector default-size 0) (make-hasheqv)))

;; Have builder construct and return a CategoricalSeries
(: complete-CategoricalSeriesBuilder (CategoricalSeriesBuilder -> CategoricalSeries))
(define (complete-CategoricalSeriesBuilder builder)
  
  (: compacted-data (Vectorof Index))
  (define compacted-data
    (let* ((data (CategoricalSeriesBuilder-data builder))
           (len (CategoricalSeriesBuilder-index builder)))
      (let: ((new-data : (Vectorof Index) (make-vector len 0)))
        ((inst vector-copy! Index) new-data 0 data 0 len)
        new-data)))
  
  (: nominals (Vectorof Symbol))
  (define nominals
    (let* ((nom-map (CategoricalSeriesBuilder-nominals builder)) 
           (len (hash-count nom-map))
           (noms (make-vector len 'NA)))
      (hash-for-each nom-map (λ: ((n : Symbol) (i : Index))
                               (vector-set! noms i n)))
      noms))
  
  (CategoricalSeries #f compacted-data nominals))

;; Extend a builder with next data element
(: append-CategoricalSeriesBuilder (CategoricalSeriesBuilder String -> Void))
(define (append-CategoricalSeriesBuilder builder str)
  
  (: nominalizer (String -> Symbol))
  (define (nominalizer str)
    (string->symbol str))
  
  (define-syntax bump
    (syntax-rules ()
      [(bump x)
       (assert (add1 x) index?)]))
  
  (define (bump-ord)
    (let ((ord (CategoricalSeriesBuilder-ord builder)))
      (set-CategoricalSeriesBuilder-ord! builder (bump ord))     
      ord))
  
  (define (bump-index)
    (let ((idx (CategoricalSeriesBuilder-index builder)))
      (set-CategoricalSeriesBuilder-index! builder (bump idx))
      idx))
  
  (: nominal-ordinal (Symbol -> Index))
  (define (nominal-ordinal sym)    
    (hash-ref! (CategoricalSeriesBuilder-nominals builder) sym bump-ord))
  
  (: extend-data (-> Void))
  (define (extend-data)
    (let* ((data (CategoricalSeriesBuilder-data builder))
           (curr-len (vector-length data))
           (new-len (assert (inexact->exact (round (* 2.0 curr-len))) exact-integer?)))
      (displayln (format "Extending from ~s to ~s" curr-len new-len))
      (let: ((new-data : (Vectorof Index) (make-vector new-len 0)))
        ((inst vector-copy! Index) new-data 0 data)
        (set-CategoricalSeriesBuilder-data! builder new-data))))
  
  (if (< (CategoricalSeriesBuilder-index builder) 
         (vector-length (CategoricalSeriesBuilder-data builder)))      
      (vector-set! (CategoricalSeriesBuilder-data builder)
                   (bump-index)
                   (nominal-ordinal (nominalizer str)))
      (begin
        (extend-data)
        (append-CategoricalSeriesBuilder builder str))))
