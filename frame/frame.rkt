#lang typed/racket/base

(provide
 Frame
 (rename-out [frame-series Frame-series])
 Frame-names Frame-dim 
 Frame-cseries Frame-nseries
 FrameSeries
 mkFrame)

(require
 (only-in racket/flonum
          flvector-length)
 (only-in "../frame/types.rkt"
          Dim)
 (only-in "series.rkt"
          Label LabelIndex LabelIndex-index
          GenericSeries 
          build-index-from-labels label-index)
 (only-in "categorical-series.rkt"
          CategoricalSeries CategoricalSeries?
          CategoricalSeries-data)
 (only-in "numeric-series.rkt"
          NumericSeries NumericSeries? 
          NumericSeries-data
          mkNumericSeries))

(define-type FrameSeries (U GenericSeries NumericSeries CategoricalSeries))

;; A frame is map of series.
(struct: Frame LabelIndex ((series : (Vectorof FrameSeries))))

(: mkFrame ((Listof (Pair Symbol FrameSeries)) -> Frame))
(define (mkFrame cols)
  (let ((index (build-index-from-labels ((inst map Label (Pair Label FrameSeries)) 
                                         (inst car Label FrameSeries) cols)))        
        (data (apply vector ((inst map FrameSeries (Pair Label FrameSeries)) cdr cols))))
    (Frame index data)))

(: frame-series (Frame Symbol -> FrameSeries))
(define (frame-series frame col)
  (vector-ref (Frame-series frame)
              (label-index (assert (LabelIndex-index frame)) col)))

(: Frame-cseries (Frame Symbol -> CategoricalSeries))
(define (Frame-cseries frame name)
  (assert (frame-series frame name) CategoricalSeries?))

(: Frame-nseries (Frame Symbol -> NumericSeries))
(define (Frame-nseries frame name)
  (assert (frame-series frame name) NumericSeries?))

(: Frame-names (Frame -> (Listof Symbol)))
(define (Frame-names frame)  
  (map (λ: ((kv : (Pair Symbol Integer)))
         (car kv))
       ((inst sort (Pair Symbol Integer) (Pair Symbol Integer))
        (hash->list (assert (LabelIndex-index frame)))        
        (λ: ((kv1 : (Pair Symbol Integer)) 
             (kv2 : (Pair Symbol Integer)))
          (< (cdr kv1) (cdr kv2))))))

(: Frame-dim (Frame -> Dim))
(define (Frame-dim frame)
  (let ((cols (length (hash-keys (assert (LabelIndex-index frame))))))
    (if (zero? cols)
        (Dim 0 0)
        (let ((rows (let ((col1 (vector-ref (Frame-series frame) 0)))
                      (cond
                        [(NumericSeries? col1) 
                         (display 'Numeric)
                         (flvector-length (NumericSeries-data col1))]
                        [(CategoricalSeries? col1)
                         (vector-length (CategoricalSeries-data col1))]
                        [else (error "Unknown Series type in Frame")]))))
          (Dim rows cols)))))



