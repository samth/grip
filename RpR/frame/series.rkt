#lang typed/racket/base

(provide:
 [gseries-count (GSeries -> Natural)])

(provide
 SIndex
 Label Label? is-labeled?
 Labeling 
 LabelIndex LabelIndex-index
 GSeries
 (struct-out GSeries)
 mkGSeries 
 series-ref series-iref
 map/GSeries 
 build-index-from-labels label-index label->idx)

(require 
 (only-in racket/flonum
          make-flvector flvector? flvector
          flvector-ref flvector-set!
          flvector-length))

(define-type Label Symbol)
(define-predicate Label? Label)

(define-type Labeling (Vectorof Label))

(define-type SIndex (HashTable Label Index))

(struct: LabelIndex ([index : (Option SIndex)]))

;; Utilities
(: build-index-from-labels ((Listof Label) -> SIndex))
(define (build-index-from-labels labels)
  (let: ((index : SIndex (make-hash '())))
    (let: loop : SIndex ((idx : Index 0) (labels : (Listof Label) labels))
      (if (null? labels)
          index
          (begin
            (hash-set! index (car labels) idx)
            (loop (assert (+ idx 1) index?) (cdr labels)))))))

(: label-index (SIndex Label -> Integer))
(define (label-index index label)      
  (hash-ref index label))

;; General Series parameterized by A
;; See NumSeries below for an optimized for Float implemenation.
(struct: (A) GSeries LabelIndex ([data : (Vectorof A)]))

(: mkGSeries (All (A) (Vectorof A) (Option (U (Listof Label) SIndex)) -> (GSeries A)))
(define (mkGSeries data labels)  
  
  (: check-mismatch (SIndex -> Void))
  (define (check-mismatch index)
    (unless (eq? (vector-length data) (hash-count index))
      (let ((k (current-continuation-marks)))
        (raise (make-exn:fail:contract "Cardinality of a Series' data and labels must be equal" k))))
    (void))
  
  (if(hash? labels)
     (begin
       (check-mismatch labels)
       (GSeries labels data))
     (if labels	 
         (let ((index (build-index-from-labels labels)))
           (check-mismatch index)
           (GSeries index data))
         (GSeries #f data))))

(: is-labeled? (LabelIndex -> Boolean))
(define (is-labeled? series)
  (if (LabelIndex-index series) #t #f))

(: label->idx (LabelIndex Label -> Index))
(define (label->idx series label)
  (let ((index (LabelIndex-index series)))
    (if index
        (hash-ref index label)
        (let ((k (current-continuation-marks)))
          (raise (make-exn:fail:contract "Cannot obtain the index of a label for a series which is unlabeled" k))))))

(: series-iref (All (A) (GSeries A) Index -> (U Float A)))
(define (series-iref series idx)
  (vector-ref (GSeries-data series) idx))

(: series-ref (All (A) (GSeries A) Label -> (U A Float)))
(define (series-ref series label)
  (series-iref series (label->idx series label)))

(: gseries-count (GSeries -> Nonnegative-Integer))
(define (gseries-count series)
  (vector-length (GSeries-data series)))

(: map/GSeries (All (A B) (GSeries A) (A -> B) -> (GSeries B)))
(define (map/GSeries series fn)
  (let*: ((old-data : (Vectorof A) (GSeries-data series))
          (new-data : (Vectorof B) (build-vector (vector-length old-data) 
                                                 (λ: ((idx : Integer)) 
                                                   (fn (vector-ref old-data idx))))))
    (GSeries (LabelIndex-index series) new-data)))


