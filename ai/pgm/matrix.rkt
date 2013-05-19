#lang typed/racket

(provide main)

(require 
 racket/unsafe/ops
 racket/fixnum
 racket/flonum)

(define-type NF Nonnegative-Fixnum)

(: matrix* : FlVector FlVector Index Index Index -> FlVector)
(define (matrix* A B m p n)
  
  (define: size : NF (fx* m n))
  (define: v : FlVector (make-flvector size))
  (define: index : NF 0)
  
  (: loop-j : NF NF -> Void)
  (define (loop-j i j)
    (when (fx< j n)
      (loop-k i j 0 0.0)
      (set! index (fx+ index 1))
      (loop-j i (fx+ j 1))))
  
  (: loop-k : NF NF NF Flonum -> Void)
  (define (loop-k i j k sum)
    (let ((i*p (fx* i p))
          (k*n (fx* k n)))
      (if (fx< k p)
          (loop-k i j (fx+ k 1)
                  (fl+ sum (fl* (unsafe-flvector-ref A (fx+ i*p k))
                                (unsafe-flvector-ref B (fx+ k*n j)))))
          (unsafe-flvector-set! v index sum))))
  
  (let: loop-i : FlVector ((i : NF 0))
    (if (< i m)
        (begin 
          (loop-j i 0)
          (loop-i (fx+ i 1)))
        v)))

(: mkFlVector (Fixnum (-> Flonum) -> FlVector))
(define (mkFlVector sz init)
  (let ((v (make-flvector sz)))
    (do ((i #{0 : Fixnum} (+ i 1)))
      ((>= i sz) v)
      (unsafe-flvector-set! v i (init)))))

(define: dim : Index 100)
(define: A : FlVector (mkFlVector (fx* dim dim) random))
(define: B : FlVector (mkFlVector (fx* dim dim) random))

(: main (-> Void))
(define (main)
  (collect-garbage)
  (collect-garbage)
  (time 
   (do ((i #{1 : Fixnum} (fx+ i 1)))
     ((fx>= i 1000) (void))
     (matrix* A B dim dim dim))
   (void)))

