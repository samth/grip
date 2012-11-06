#lang typed/racket/base

(provide
 (struct-out Tabulation)
 (struct-out Binning))

(provide:
 [categorize (NumericSeries Binning -> CategoricalSeries)]
 [tabulate (CategoricalSeries -> Tabulation)]
 [ntabulate (NumericSeries [#:algo Binning-Algo] -> NTabulation)]
 [ntabulation->tabulation (NTabulation -> Tabulation)])

(require 
 (only-in "../frame/series.rkt"
          Label))
  
  (: generate-anon-labels (Integer -> (Listof Label)))
  (define (generate-anon-labels n)
    
    ;; Generate A ... Z, AA ... ZZ, AAA ... ZZZ, ...
    (: generate-anon-varid (Integer -> Symbol))
    (define (generate-anon-varid id)
      (: gen-name (Integer -> String))
      (define (gen-name i)
        (define A-base 65)
        (define letters 26)
        (let-values (((q r) (quotient/remainder i letters)))
          (let ((ch (integer->char (+ r A-base))))
            (make-string (add1 q) ch))))
      (string->symbol (gen-name id)))
      
    (for/list ([i (in-range n)])
      (generate-anon-varid i)))

(require
 racket/pretty
 racket/match
 (only-in prelude/std/prelude
          vadd1)
 (only-in racket/flonum
          make-flvector
          flvector-ref
          flvector-length)
 (only-in "../stats/statistics.rkt"
          summary
          Summary Summary-count Summary-max Summary-min)
 (only-in "../frame/series.rkt"
          Labeling)
 (only-in "../frame/categorical-series.rkt"
          CategoricalSeries
          CategoricalSeries-data
          CategoricalSeries-nominals)
 (only-in "../frame/numeric-series.rkt"
          NumericSeries NumericSeries-data)
 (only-in "../frame/gen-nseries.rkt"
          flvector-print
          generate-NumericSeries))

(require/typed racket
  [vector-copy (All (A) ((Vectorof A) -> (Vectorof A)))])

(struct: Tabulation ([nominals : (Vectorof Symbol)]
                     [counts : (Vectorof Natural)]) #:transparent)

(struct: Binning ([breaks : FlVector]
                  [nominals : Labeling]) #:transparent)

(: tabulate (CategoricalSeries -> Tabulation))
(define (tabulate catseries)    
  (let ((nominals (CategoricalSeries-nominals catseries))
        (data      (CategoricalSeries-data catseries)))
    (let ((noms-sz (vector-length nominals))
          (data-len (vector-length data)))
      (let: ((cnts : (Vectorof Natural)    (make-vector noms-sz 0))
             (nominals : (Vectorof Symbol) (vector-copy nominals)))
        (do ([i 0 (add1 i)])
          ([>= i data-len] (Tabulation nominals cnts))
          (vadd1 cnts (vector-ref data i)))))))

;; Numeric Series Tabulations

;; Assumes continous widths of a fixed size from start.
;; Bins are always open on the left and fixed on the right.
;; e.g Start = 20 Width = 10 (20,30] (30,40] ... for count

(struct: NTabulation ([start  : Float]
                      [width  : Float]
                      [counts : (Vectorof Natural)]) #:transparent)

(define-type Binning-Algo
  (U 'Fixed 'Struges 'Doane 'Scott 'Sqr 'Freedman-Diaconis))

(: determine-width-or-bin-count (Float Float (U Float Integer) -> Integer))
(define (determine-width-or-bin-count min max width-or-bin-count)
  (let ((wbc (if (exact? width-or-bin-count)
                 (exact->inexact width-or-bin-count)
                 width-or-bin-count)))
    (assert (inexact->exact (ceiling (/ (- max min) wbc))) exact-integer?)))

(: strudges-bin-count (Nonnegative-Integer -> Integer))
(define (strudges-bin-count n)
  (assert (inexact->exact (ceiling (+ 1.0 (/ (log n) (log 2))))) exact-integer?))

(: determine-binning (Binning-Algo Summary 
                                   [#:width (Option Nonnegative-Integer)]
                                   [#:nominals (Listof Label)]
                                   -> Binning))
(define (determine-binning algo summary #:width [bin-cnt #f] #:nominals [nominals '()])
  (match summary
    [(Summary mean variance min-x max-x count _)
     (case algo
       ([Fixed] (if bin-cnt
                    (let* ((width (determine-width-or-bin-count min-x max-x bin-cnt))
                           (breaks (NumericSeries-data (generate-NumericSeries min-x max-x #:by (exact->inexact width)))))
                      (Binning breaks (list->vector (generate-anon-labels (flvector-length breaks)))))
                    (error "Fixed width algo requires #:width value to be provided.")))
       (else (error "Unhandled binning algo (can't happend)")))]))
                    
(: pigeon-by-breaks (FlVector Float -> Integer))
(define (pigeon-by-breaks breaks x)    
  (let ((max-slot (sub1 (flvector-length breaks))))
    (when (or (< x (flvector-ref breaks 0))
              (> x (flvector-ref breaks max-slot)))
      (error (format "Data value ~s not in histogram bin ranges [~s,~s]." 
                     x (flvector-ref breaks 0) (flvector-ref breaks max-slot))))
    (let ((len (flvector-length breaks)))
      (let binary-search ((i-min 0) (i-max (sub1 (flvector-length breaks))))      
        (if (< i-max i-min)
            0
            (let ((i-mid (truncate (+ i-min (/ (- i-max i-min) 2)))))
              (if (<= x (flvector-ref breaks i-mid))
                  (binary-search i-min (sub1 i-mid))
                  (if (> x (flvector-ref breaks (add1 i-mid)))
                      (binary-search (add1 i-mid) i-max)
                      i-mid))))))))

(: ntabulate (NumericSeries [#:algo Binning-Algo] -> NTabulation))
(define (ntabulate nseries #:algo [algo 'Struges])  
  (define data (NumericSeries-data nseries))
  (define data-length (flvector-length data))
  (define summary-data (summary nseries))
  (define min-x (floor (Summary-min summary-data)))
  (define max-x (ceiling (Summary-max summary-data)))
  (define bin-count (strudges-bin-count (Summary-count summary-data)))
  (define bin-width (exact->inexact (determine-width-or-bin-count min-x max-x bin-count)))
  (define breaks (NumericSeries-data (generate-NumericSeries min-x max-x #:by bin-width)))  
  (define: counts : (Vectorof Natural) (make-vector bin-count 0))  
  (do ([i 0 (add1 i)])
    ([>= i data-length] (NTabulation min-x bin-width counts))    
    (vadd1 counts (pigeon-by-breaks breaks (flvector-ref data i)))))

(: categorize (NumericSeries Binning -> CategoricalSeries))
(define (categorize nseries binning)
  (define data (NumericSeries-data nseries))
  (define data-len (flvector-length data))
  (define: nominal-data : (Vectorof Index) (make-vector data-len 0))  
  (match binning
    [(Binning breaks nominals)
     (do ([i 0 (add1 i)])
       ([>= i data-len] (CategoricalSeries #f nominal-data nominals))
       (displayln (format "~s :: ~s" (flvector-ref data i) (pigeon-by-breaks breaks (flvector-ref data i))))
       (vector-set! nominal-data i (assert (pigeon-by-breaks breaks (flvector-ref data i)) index?)))]))

(: ntabulation->tabulation (NTabulation -> Tabulation))
(define (ntabulation->tabulation ntab)
  (let ((counts (NTabulation-counts ntab)))
    (Tabulation  (list->vector (generate-anon-labels (vector-length counts))) counts)))