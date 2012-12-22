#lang typed/racket/base

(provide:
 [frame-description (Frame -> FrameDescription)]
 [show-frame-description (FrameDescription -> Void)])

(provide
 Frame
 frame-series
 frame-names frame-dim 
 frame-cseries frame-nseries
 frame-series
 mkFrame)

(require 
 (only-in "../frame/types.rkt"
          Dim Dim-rows Dim-cols)
 (only-in "series.rkt"
          Label LabelIndex LabelIndex-index
          GSeries 
          build-index-from-labels label-index)
 (only-in "series-description.rkt"
          series-count
          frame-series-type-label
          Series 
          SeriesDescription SeriesDescription-name
          SeriesDescription-type SeriesDescription-count)
 (only-in "categorical-series.rkt"
          CSeries CSeries?
          CSeries-data)
 (only-in "numeric-series.rkt"
          NSeries NSeries? 
          NSeries-data
          mkNSeries))

;; A frame is map of series.
(struct: Frame LabelIndex ([series : (Vectorof Series)]))

(struct: FrameDescription ([dimensions : Dim]
                           [series : (Listof SeriesDescription)]))
                           

(: mkFrame ((Listof (Pair Symbol Series)) -> Frame))
(define (mkFrame cols)
  (let ((index (build-index-from-labels ((inst map Label (Pair Label Series)) 
                                         (inst car Label Series) cols)))        
        (data (apply vector ((inst map Series (Pair Label Series)) cdr cols))))
    (Frame index data)))

(: frame-series (Frame Symbol -> Series))
(define (frame-series frame col)
  (vector-ref (Frame-series frame)
              (label-index (assert (LabelIndex-index frame)) col)))

(: frame-cseries (Frame Symbol -> CSeries))
(define (frame-cseries frame name)
  (assert (frame-series frame name) CSeries?))

(: frame-nseries (Frame Symbol -> NSeries))
(define (frame-nseries frame name)
  (assert (frame-series frame name) NSeries?))

(: frame-names (Frame -> (Listof Symbol)))
(define (frame-names frame)  
  (map (λ: ((kv : (Pair Symbol Integer)))
         (car kv))
       ((inst sort (Pair Symbol Integer) (Pair Symbol Integer))
        (hash->list (assert (LabelIndex-index frame)))        
        (λ: ((kv1 : (Pair Symbol Integer)) 
             (kv2 : (Pair Symbol Integer)))
          (< (cdr kv1) (cdr kv2))))))

(: frame-dim (Frame -> Dim))
(define (frame-dim frame)
  (let ((cols (length (hash-keys (assert (LabelIndex-index frame))))))
    (if (zero? cols)
        (Dim 0 0)
        (let ((rows (let ((series (vector-ref (Frame-series frame) 0)))
                      (series-count series))))                      
          (Dim rows cols)))))

(: frame-description (Frame -> FrameDescription))
(define (frame-description frame)
  (let ((names (frame-names frame)))
    (let: loop : FrameDescription ((names : (Listof Label) names) (descs : (Listof SeriesDescription) '()))
      (if (null? names)
          (FrameDescription (frame-dim frame) (reverse descs))
          (let* ((name (car names))
                 (series (frame-series frame name)))
            (loop (cdr names) (cons (SeriesDescription name 
                                                       (frame-series-type-label series) 
                                                       (series-count series))
                                    descs)))))))

;; Really need to enumerate a minimal set of generic functions, such as `show'
(: show-frame-description (FrameDescription -> Void))
(define (show-frame-description fdesc)
  
  (: print-series-description (SeriesDescription -> Void))
  (define (print-series-description sdesc)
   (displayln (format "  - ~a: ~a"
                      (SeriesDescription-name sdesc)
                      (SeriesDescription-type sdesc))))                      
  
  (let ((dim (FrameDescription-dimensions fdesc)))
    (displayln (format "Frame::(Cols: ~a, Rows: ~a)" (Dim-cols dim) (Dim-rows dim)))
    (for-each print-series-description (FrameDescription-series fdesc))))
            