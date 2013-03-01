#lang typed/racket/base

(provide
 (struct-out ISeries))

(provide:
 [new-ISeries ((Vectorof Fixnum) (Option (U (Listof Label) SIndex)) -> ISeries)]
 [iseries-ref (ISeries Index -> Fixnum)]
 [iseries-count (ISeries -> Index)])

(require 
 (only-in "indexed-series.rkt"
	  build-index-from-labels
	  Label SIndex LabelIndex))

(struct: ISeries LabelIndex ([data : (Vectorof Fixnum)]))

(: new-ISeries ((Vectorof Fixnum) (Option (U (Listof Label) SIndex)) -> ISeries))

(define (new-ISeries data labels)
  
  (: check-mismatch (SIndex -> Void))
  (define (check-mismatch index)
    (unless (eq? (vector-length data) (hash-count index))
      (let ((k (current-continuation-marks)))
	(raise (make-exn:fail:contract "Cardinality of a Series' data and labels must be equal" k))))
    (void))
  
  (if (hash? labels)
      (begin
	(check-mismatch labels)
	(ISeries labels data))
      (if labels
	  (let ((index (build-index-from-labels labels)))
	    (check-mismatch index)
	    (ISeries index data))
	  (ISeries #f data))))

(: iseries-ref (ISeries Index -> Fixnum))
(define (iseries-ref series idx)
  (vector-ref (ISeries-data series) idx))

(: iseries-count (ISeries -> Index))
(define (iseries-count series)
  (vector-length (ISeries-data series)))
