#lang typed/racket/base

(provide
 textfield
 tab-split-text-into-fields
 split-textfields-at
 project-textfields)

(require
 (only-in racket/vector
          vector-split-at)
 (only-in "../types.rkt"
          Text TextFields))         

(: tab-split-text-into-fields (Text -> TextFields))
(define (tab-split-text-into-fields text)
  (list->vector (regexp-split #rx"\t" text)))

(: textfield (TextFields Index -> Text))
(define (textfield fields idx)
  (vector-ref fields idx))

(: split-textfields-at (TextFields Index -> (Values TextFields TextFields)))
(define (split-textfields-at fields idx)
  (vector-split-at fields idx))
          
;; Project out the given "columns" of data.
(: project-textfields (TextFields (Vectorof Integer) -> TextFields))
(define (project-textfields fields field-idxs)     
  (define idx-cnt  (vector-length field-idxs))
  (define field-cnt (vector-length fields))  
  (define: projection : (Vectorof String) (make-vector idx-cnt ""))
  
  (do ((i 0 (add1 i)))
    ((>= i idx-cnt) projection)
    (let ((idx (sub1 (vector-ref field-idxs i))))
      (when (and (>= idx 0) (< idx field-cnt))
        (vector-set! projection i (vector-ref fields idx))))))