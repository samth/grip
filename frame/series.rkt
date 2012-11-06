#lang typed/racket/base

(provide
 SIndex
 Label Label? is-labeled?
 Labeling 
 LabelIndex LabelIndex-index
 GenericSeries
 (struct-out GenericSeries)
 mkGenericSeries 
 series-ref series-iref
 map/GenericSeries 
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
(struct: (A) GenericSeries LabelIndex ([data : (Vectorof A)]))

(: mkGenericSeries (All (A) (Vectorof A) (Option (U (Listof Label) SIndex)) -> (GenericSeries A)))
(define (mkGenericSeries data labels)  
  
  (: check-mismatch (SIndex -> Void))
  (define (check-mismatch index)
    (unless (eq? (vector-length data) (hash-count index))
      (let ((k (current-continuation-marks)))
        (raise (make-exn:fail:contract "Cardinality of a Series' data and labels must be equal" k))))
    (void))
  
  (if(hash? labels)
     (begin
       (check-mismatch labels)
       (GenericSeries labels data))
     (if labels	 
         (let ((index (build-index-from-labels labels)))
           (check-mismatch index)
           (GenericSeries index data))
         (GenericSeries #f data))))

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

(: series-iref (All (A) (GenericSeries A) Index -> (U Float A)))
(define (series-iref series idx)
  (vector-ref (GenericSeries-data series) idx))

(: series-ref (All (A) (GenericSeries A) Label -> (U A Float)))
(define (series-ref series label)
  (series-iref series (label->idx series label)))

(: map/GenericSeries (All (A B) (GenericSeries A) (A -> B) -> (GenericSeries B)))
(define (map/GenericSeries series fn)
  (let*: ((old-data : (Vectorof A) (GenericSeries-data series))
          (new-data : (Vectorof B) (build-vector (vector-length old-data) 
                                                 (λ: ((idx : Integer)) 
                                                   (fn (vector-ref old-data idx))))))
    (GenericSeries (LabelIndex-index series) new-data)))


