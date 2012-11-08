#lang typed/racket/base

(provide
 CSeriesBuilder
 CSeriesBuilder?
 mkCSeriesBuilder
 append-CSeriesBuilder
 complete-CSeriesBuilder)

(require
 (only-in "../frame/categorical-series.rkt"
          CSeries))

(struct: CSeriesBuilder ([index : Index]
                                   [ord : Index]
                                   [data : (Vectorof Index)]
                                   [nominals : (HashTable Symbol Index)]) #:mutable #:transparent)

;; Make a SeriesBuilder for a CSeries
(: mkCSeriesBuilder (-> CSeriesBuilder))
(define (mkCSeriesBuilder)
  (define default-size 32)
  (CSeriesBuilder 0 0 (make-vector default-size 0) (make-hasheqv)))

;; Have builder construct and return a CSeries
(: complete-CSeriesBuilder (CSeriesBuilder -> CSeries))
(define (complete-CSeriesBuilder builder)
  
  (: compacted-data (Vectorof Index))
  (define compacted-data
    (let* ((data (CSeriesBuilder-data builder))
           (len (CSeriesBuilder-index builder)))
      (let: ((new-data : (Vectorof Index) (make-vector len 0)))
        ((inst vector-copy! Index) new-data 0 data 0 len)
        new-data)))
  
  (: nominals (Vectorof Symbol))
  (define nominals
    (let* ((nom-map (CSeriesBuilder-nominals builder)) 
           (len (hash-count nom-map))
           (noms (make-vector len 'NA)))
      (hash-for-each nom-map (λ: ((n : Symbol) (i : Index))
                               (vector-set! noms i n)))
      noms))
  
  (CSeries #f compacted-data nominals))

;; Extend a builder with next data element
(: append-CSeriesBuilder (CSeriesBuilder String -> Void))
(define (append-CSeriesBuilder builder str)
  
  (: nominalizer (String -> Symbol))
  (define (nominalizer str)
    (string->symbol str))
  
  (define-syntax bump
    (syntax-rules ()
      [(bump x)
       (assert (add1 x) index?)]))
  
  (define (bump-ord)
    (let ((ord (CSeriesBuilder-ord builder)))
      (set-CSeriesBuilder-ord! builder (bump ord))     
      ord))
  
  (define (bump-index)
    (let ((idx (CSeriesBuilder-index builder)))
      (set-CSeriesBuilder-index! builder (bump idx))
      idx))
  
  (: nominal-ordinal (Symbol -> Index))
  (define (nominal-ordinal sym)    
    (hash-ref! (CSeriesBuilder-nominals builder) sym bump-ord))
  
  (: extend-data (-> Void))
  (define (extend-data)
    (let* ((data (CSeriesBuilder-data builder))
           (curr-len (vector-length data))
           (new-len (assert (inexact->exact (round (* 2.0 curr-len))) exact-integer?)))
      (displayln (format "Extending from ~s to ~s" curr-len new-len))
      (let: ((new-data : (Vectorof Index) (make-vector new-len 0)))
        ((inst vector-copy! Index) new-data 0 data)
        (set-CSeriesBuilder-data! builder new-data))))
  
  (if (< (CSeriesBuilder-index builder) 
         (vector-length (CSeriesBuilder-data builder)))      
      (vector-set! (CSeriesBuilder-data builder)
                   (bump-index)
                   (nominal-ordinal (nominalizer str)))
      (begin
        (extend-data)
        (append-CSeriesBuilder builder str))))
