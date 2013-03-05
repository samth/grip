#lang typed/racket/base

(provide
 (struct-out Summary))

(provide:
 [mean (FlVector -> Float)]
 [variance (NSeries -> Float)]
 [stddev   (NSeries -> Float)]
 [summary  (NSeries -> Summary)])

(require
 (only-in racket/flonum
          flvector-ref
          flvector-length
          flsqrt)
 (only-in "../frame/numeric-series.rkt"
           NSeries NSeries-data))

(struct: Summary ([mean : Float]
                  [variance : Float]
                  [min : Float]
                  [max : Float]
                  [count : Natural]
                  [nans : Natural]))

;; See Knuth TAOCP vol 2, 3rd edition, page 232
;; For mean and variance.
;; Initialize M1 = x1 and S1 = 0.
;; For subsequent x's, use the recurrence formulas
;; Mk = Mk-1+ (xk - Mk-1)/k 
;; Sk = Sk-1 + (xk - Mk-1)*(xk - Mk).

;; It is an error to attempt to find the mean of an empty vector.
;; The mean of a vector of all NaNs is a Nan.
(: mean (FlVector -> Float))
(define (mean data) 
  (let ((len (flvector-length data)))
    (unless (> len 0)
      (raise (make-exn:fail:contract "Empty vector for mean calculation" 
                                     (current-continuation-marks))))
    (if (= len 1)
        (flvector-ref data 0)
        (let ((m1 (let ((m (flvector-ref data 0)))
                    (if (eqv? m +nan.0) 0.0 m))))
          (let loop ((i 1) (k 2) (m m1))
            (if (< i len)
                (let ((x (flvector-ref data i)))
                  (if (eqv? x +nan.0)
                      (loop (add1 i) k m)
                      (loop (add1 i) (add1 k) (+ m (/ (- x m) k)))))
                m))))))

(: summary (NSeries -> Summary))
(define (summary nseries)
  
  (define data (NSeries-data nseries))
  (define: min-x : Float (flvector-ref data 0))  
  (define: max-x : Float min-x)
  (define: cnt   : Natural (if (eqv? +nan.0 min-x) 0 1))
  (define: nans  : Natural (if (eqv? +nan.0 min-x) 1 0))
     
  (let ((len (flvector-length data)))
    (unless (> len 0)
      (raise (make-exn:fail:contract "Empty vector for mean, variance calculation" 
                                     (current-continuation-marks))))
    (let loop ((i 1) (k 2) (m (flvector-ref data 0)) (s 0.0))
      (if (< i len)
          (let ((x (flvector-ref data i)))
            (if (eqv? x +nan.0)
                (begin                 
                  (set! nans (add1 nans))
                  (loop (add1 i) k m s))
                (begin
                  (set! cnt (add1 cnt))
                  (set! min-x (min x min-x))
                  (set! max-x (max x max-x))                
                  (let ((new-m (+ m (/ (- x m) k))))
                    (loop (add1 i) (add1 k) new-m (+ s (* (- x m) (- x new-m))))))))
          (if (> k 2)
              (Summary m (/ s (- k 2)) min-x max-x cnt nans)              
              (Summary min-x 0.0 (min min-x 0.0) (max min-x 0.0) cnt nans))))))

(: variance (NSeries -> Float))
(define (variance nseries)
  (Summary-variance (summary nseries)))

(: stddev (NSeries -> Float))
(define (stddev nseries)
  (flsqrt (variance nseries)))
