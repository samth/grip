#lang typed/racket/base

;; Mutual Information Determination
;; Learning Bayesian Networks From Data:
;;  An Efficient Approach Based on Information Theory
;;  Jie Cheng, David Bell, Weiru Liu

;; While this could have been written much more succintly using Tabulation of a CategoricalSeries
;; and CrossTabulation of two CategoricalSeries each involves their own individual passes over the data.

;; One approach is to go with an Iteratee approach which allows combining of Iteratees 
;; with a single (pass) Enumeration of the data.

;; The other is the ol' cut and paste the logic in CrossTabulation and Tabulation to achieve a single 
;; pass over data MutualInformation calculation.

(provide 
 (struct-out MutualInformation)
 mutual-information)

(require 
 (only-in prelude/std/prelude
          vadd1)
 (only-in "../../RpR/frame/series.rkt"
          SIndex)
 (only-in "../../RpR/frame/categorical-series.rkt"
          CategoricalSeries
          CategoricalSeries-nominals
          CategoricalSeries-data)          
 (only-in "../../RpR/stats/tabulate.rkt"
          Tabulation)
 (only-in "../pgm/xtab.rkt"
          CrossTabulation))

(struct: MutualInformation ([phi : Float]
                            [nom1 : Tabulation]
                            [nom2 : Tabulation]
                            [xtab : CrossTabulation]) #:transparent)

(: mutual-information (CategoricalSeries CategoricalSeries -> MutualInformation))
(define (mutual-information cs1 cs2)
  
  (define d1 (CategoricalSeries-data cs1)) 
  (define d2 (CategoricalSeries-data cs2))  
  
  (define: d1-nom-cnt : Index (assert (vector-length (CategoricalSeries-nominals cs1)) index?)) 
  (define: d2-nom-cnt : Index (assert (vector-length (CategoricalSeries-nominals cs2)) index?))
  
  (define: stride : Index d1-nom-cnt)
  (define: len : Index (vector-length d1))
  
  (define: d1-counts : (Vectorof Natural) (make-vector d1-nom-cnt 0))    
  (define: d2-counts : (Vectorof Natural) (make-vector d2-nom-cnt 0))
  
  (define: xtab-counts : (Vectorof Integer) (make-vector (* d1-nom-cnt d2-nom-cnt) 0))
  
  (: assignment-to-index (Integer Integer -> Integer))
  (define (assignment-to-index a b)
    (+ a (* b stride)))
  
  (: partition ((Vectorof Natural) -> Float))
  (define (partition counts)
    (define len (vector-length counts))
    (define: p : Natural 0)
    (do ([i 0 (add1 i)])
      ([>= i len] (exact->inexact p))
      (set! p (+ p (vector-ref counts i)))))
  
  ;; Really UGLY for performance.
  (: phi (-> Float))
  (define (phi)
    (define: minfo : Float 0.0)
    (define: P : Float (partition d1-counts))
    (do ([i 0 (add1 i)])     
      ([>= i d1-nom-cnt])
      (do ([j 0 (add1 j)])
        ([>= j d2-nom-cnt])           
        (let ((PXiXj (/ (vector-ref xtab-counts (assignment-to-index i j)) P))
              (PXi   (/ (vector-ref d1-counts i) P))
              (PXj   (/ (vector-ref d2-counts j) P)))
          (when (and
                 (> PXiXj 0.0)
                 (> PXi 0.0)                
                 (> PXj 0.0))           
            (let ((result (assert (* PXiXj (/ (log (/ PXiXj (* PXi PXj))) (log 2))) flonum?)))
              (set! minfo (+ minfo result)))))))
    minfo)
  
  (unless (eq? len (vector-length d2))
    (error "Cannot determine the mutual information for two CategoricalSeries with differing data lengths."))
  
  (define-syntax bump
    (syntax-rules ()
      ((bump v idx)
       (vector-set! v idx (add1 (vector-ref v idx))))))
  
  (do ([idx 0 (add1 idx)])
    ([>= idx len] (MutualInformation
                   (phi)
                   ;;0.0
                   (Tabulation (CategoricalSeries-nominals cs1) d1-counts)
                   (Tabulation (CategoricalSeries-nominals cs2) d2-counts)
                   (CrossTabulation (CategoricalSeries-nominals cs1)
                                    (CategoricalSeries-nominals cs2)
                                    xtab-counts)))
    (let* ((d1-val (vector-ref d1 idx))
           (d2-val (vector-ref d2 idx))
           (j (assignment-to-index d1-val d2-val)))
      (bump xtab-counts j)      
      (vadd1 d1-counts d1-val)
      (vadd1 d2-counts d2-val))))


(: test (-> MutualInformation))
(define (test)
  (define cs1 
    (CategoricalSeries #f
                       ;; '#(1 0 0)
                       '#(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 1 1 1 1 1 1 1 2 2 2 2 3 3 3 3)
                       '#[A1 A2 A3 A4]))
  (define cs2
    (CategoricalSeries #f
                       ;;'#(0 1 0 0 1 0 0 1 0 0)
                       ;; '#(1 0 1)                                              
                       '#(0 0 0 0 1 1 2 2 3 3 3 3 3 3 3 3 0 0 1 1 1 1 2 2 0 1 2 2 0 1 2 2)
                       '#(B1 B2 B3 B4)))
  (mutual-information cs1 cs2))
