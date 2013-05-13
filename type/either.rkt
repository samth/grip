#lang typed/racket/base

(provide
 Either
 (struct-out Left)
 (struct-out Right))

(provide:
 [left  (All (E D) (Either E D) -> E)]
 [right (All (E D) (Either E D) -> D)])

(struct: (E) Left  ([val : E]) #:transparent)
(struct: (D) Right ([val : D]) #:transparent)

(define-type (Either E D) (U (Left E) (Right D)))

(: either (All (E D T) ((E -> T) (D -> T) (Either E D) -> T)))
(define (either left right an-either)
  (if (Right? an-either)
      (right (Right-val an-either))
      (left  (Left-val an-either))))

(: left (All (E D) (Either E D) -> E))
(define (left either)
  (if (Left? either)
      (Left-val either)
      (error 'left "Attempting to extract Left value from a Right Either: ~a" either)))

(: right (All (E D) (Either E D) -> D))
(define (right either)
  (if (Right? either)
      (Right-val either)
      (error 'right "Attempting to extract Right value from a Left Either: ~a" either)))
