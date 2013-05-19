#lang typed/racket

(provide main)

(require racket/unsafe/ops
         racket/flonum)

(define-type NF Nonnegative-Fixnum)

(: matrix* :
   (Vectorof Flonum) (Vectorof Flonum) Index Index Index
   -> (Vectorof Flonum))
(define (matrix* A B m p n)
  (define size (* m n))
  (define: v : (Vectorof Flonum) (make-vector size 0.0))
  (define index 0)
  (: loop-i : NF -> (Vectorof Flonum))
  (define (loop-i i)
    (cond [(< i m)
           (loop-j i 0)
           (loop-i (+ i 1))]
          [else v]))
  (: loop-j : NF NF -> Void)
  (define (loop-j i j)
    (when (< j n)
      (loop-k i j 0 0.0)
      (set! index (+ index 1))
      (loop-j i (+ j 1))))
  (: loop-k : NF NF NF Flonum -> Void)
  (define (loop-k i j k sum)
    (define i*p (* i p))
    (define k*n (* k n))
    (cond
      [(< k p)
       (loop-k i j (+ k 1)
               (+ sum
                  (* (unsafe-vector-ref A (+ i*p k))
                     (unsafe-vector-ref B (+ k*n j)))))]
      [else
       (vector-set! v index sum)]))
  (loop-i 0))

(define dim 250)
(define A (build-vector (* dim dim) (λ (_) (random))))
(define B (build-vector (* dim dim) (λ (_) (random))))

(define (main)
  (collect-garbage)
  (collect-garbage)
  
  (let ((m (time (matrix* A B dim dim dim))))
    (void)))
