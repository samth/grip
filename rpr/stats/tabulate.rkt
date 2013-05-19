#lang typed/racket/base

(provide
 (struct-out Tabulation)
 (struct-out Binning))

(provide:
 [categorize (NSeries Binning -> CSeries)]
 [tabulate (CSeries -> Tabulation)]
 [ntabulate (NSeries [#:algo Binning-Algo] -> NTabulation)]
 [ntabulation->tabulation (NTabulation -> Tabulation)]
 [frame-cseries-tabulate (Frame Symbol -> Frame)])

(require 
 (only-in "../frame/indexed-series.rkt"
          Label)
 (only-in "../frame/categorical-series.rkt"
	  new-CSeries)
 (only-in "../frame/integer-series.rkt"
	  new-ISeries)
 (only-in "../frame/frame.rkt"
	  Frame
	  new-frame
	  frame-cseries))
	    
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
 (only-in type/prelude
          vadd1)
 (only-in racket/flonum
          make-flvector
          flvector-ref
          flvector-length)
 (only-in "../stats/statistics.rkt"
          summary
          Summary Summary-count Summary-max Summary-min)
 (only-in "../frame/categorical-series.rkt"
          CSeries
          CSeries-data
          CSeries-nominals)
 (only-in "../frame/numeric-series.rkt"
          NSeries NSeries-data)
 (only-in "../frame/gen-nseries.rkt"
          flvector-print
          generate-NSeries))

(require/typed racket
  [vector-copy (All (A) ((Vectorof A) -> (Vectorof A)))])

(struct: Tabulation ([nominals : (Vectorof Symbol)]
                     [counts : (Vectorof Fixnum)]))

(struct: Binning ([breaks : FlVector]
                  [nominals : (Vectorof Label)]))

(: frame-cseries-tabulate  (Frame Symbol -> Frame))
(define (frame-cseries-tabulate frame symbol)
  (let ((tab (tabulate (frame-cseries frame symbol))))
    (new-frame (list (cons symbol (new-CSeries (Tabulation-nominals tab)))
		     (cons 'freq (new-ISeries (Tabulation-counts tab) #f))))))

(: tabulate (CSeries -> Tabulation))
(define (tabulate catseries)    
  (let ((nominals (CSeries-nominals catseries))
        (data      (CSeries-data catseries)))
    (let ((noms-sz (vector-length nominals))
          (data-len (vector-length data)))
      (let: ((cnts : (Vectorof Fixnum)    (make-vector noms-sz 0))
             (nominals : (Vectorof Symbol) (vector-copy nominals)))
        (do ([i 0 (add1 i)])
          ([>= i data-len] (Tabulation nominals cnts))
	  (let ((idx (vector-ref data i)))
	    (vector-set! cnts idx (assert (add1 (vector-ref cnts idx)) index?))))))))

;; Numeric Series Tabulations

;; Assumes continous widths of a fixed size from start.
;; Bins are always open on the left and fixed on the right.
;; e.g Start = 20 Width = 10 (20,30] (30,40] ... for count

(struct: NTabulation ([start  : Float]
                      [width  : Float]
                      [counts : (Vectorof Fixnum)]))

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
                           (breaks (NSeries-data (generate-NSeries min-x max-x #:by (exact->inexact width)))))
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

(: ntabulate (NSeries [#:algo Binning-Algo] -> NTabulation))
(define (ntabulate nseries #:algo [algo 'Struges])  
  (define data (NSeries-data nseries))
  (define data-length (flvector-length data))
  (define summary-data (summary nseries))
  (define min-x (floor (Summary-min summary-data)))
  (define max-x (ceiling (Summary-max summary-data)))
  (define bin-count (strudges-bin-count (Summary-count summary-data)))
  (define bin-width (exact->inexact (determine-width-or-bin-count min-x max-x bin-count)))
  (define breaks (NSeries-data (generate-NSeries min-x max-x #:by bin-width)))  
  (define: counts : (Vectorof Fixnum) (make-vector bin-count 0))  
  (do ([i 0 (add1 i)])
    ([>= i data-length] (NTabulation min-x bin-width counts))    
    (let ((idx (pigeon-by-breaks breaks (flvector-ref data i))))
      (vector-set! counts idx (assert (add1 (vector-ref counts idx)) index?)))))

(: categorize (NSeries Binning -> CSeries))
(define (categorize nseries binning)
  (define data (NSeries-data nseries))
  (define data-len (flvector-length data))
  (define: nominal-data : (Vectorof Index) (make-vector data-len 0))  
  (match binning
    [(Binning breaks nominals)
     (do ([i 0 (add1 i)])
       ([>= i data-len] (CSeries #f nominal-data nominals))
       (displayln (format "~s :: ~s" (flvector-ref data i) (pigeon-by-breaks breaks (flvector-ref data i))))
       (vector-set! nominal-data i (assert (pigeon-by-breaks breaks (flvector-ref data i)) index?)))]))

(: ntabulation->tabulation (NTabulation -> Tabulation))
(define (ntabulation->tabulation ntab)
  (let ((counts (NTabulation-counts ntab)))
    (Tabulation  (list->vector (generate-anon-labels (vector-length counts))) counts)))
