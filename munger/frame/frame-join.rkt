;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Ray Racine's Data Munger Library
;; Copyright (C) 2007-2013  Raymond Paul Racine
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#lang typed/racket/base

(provide:
 [frame-append (Frame Frame [#:col (Listof Symbol)] -> Frame)]
 [frame-merge (Frame Frame [#:on (Listof Symbol)] -> Frame)])

(require
 racket/pretty
 racket/unsafe/ops
 (only-in racket/set
	  set set-member?
	  list->set set->list
	  set-intersect set-subtract)
 (only-in type/symbol
	  symbol-prefix)
 (only-in "indexed-series.rkt"
	  Label Labeling LabelProjection)
 (only-in "series.rkt"
	  series-complete)
 (only-in "series-description.rkt"
	  SeriesType Series
	  SeriesDescription-type
	  series-type series-count)
 (only-in "frame.rkt"
	  Frame new-frame frame-names
	  frame-cseries frame-explode
	  FrameDescription FrameDescription-series frame-description)
 (only-in "numeric-series.rkt"
	  NSeries NSeries? nseries-ref)
 (only-in "integer-series.rkt"
	  ISeries ISeries? iseries-ref
	  iseries-referencer)
 (only-in "categorical-series.rkt"
	  cseries-referencer cseries-count cseries-ref
	  CSeries CSeries?)
 (only-in "series-builder.rkt"
	  SeriesBuilder)
 (only-in "integer-series-builder.rkt"
	  ISeriesBuilder ISeriesBuilder?
	  append-ISeriesBuilder complete-ISeriesBuilder
	  new-ISeriesBuilder)
 (only-in "categorical-series-builder.rkt"
	  CSeriesBuilder CSeriesBuilder?
	  append-CSeriesBuilder complete-CSeriesBuilder
	  new-CSeriesBuilder)
 (only-in "categorical-series-ops.rkt"
	  cseries-append)
 (only-in "numeric-series-builder.rkt"
	  NSeriesBuilder NSeriesBuilder?
	  append-NSeriesBuilder complete-NSeriesBuilder
	  new-NSeriesBuilder))

(define-type Column (Pair Label Series))
(define-type Key String)
(define-type JoinHash (HashTable Key (Listof Index)))
(define-type IndexableSeries (U CSeries ISeries))

(define key-delimiter "\t")

(: column-series (Column -> Series))
(define (column-series scol)
  (cdr scol))

(: join-column-name (Column (Setof Label) String -> Symbol))
(define (join-column-name column common-cols prefix)
  (let ((colname (car column)))
    (if (set-member? common-cols colname)
	(symbol-prefix colname prefix)
	colname)))

(: dest-mapping-series-builders (FrameDescription Index -> (Listof SeriesBuilder)))
(define (dest-mapping-series-builders frame-description len)
  (for/list: : (Listof SeriesBuilder)
	     ([series (FrameDescription-series frame-description)])
	     (case (SeriesDescription-type series)
	       ((CategoricalSeries) (new-CSeriesBuilder len))
	       ((NumericSeries)     (new-NSeriesBuilder len))
	       ((IntegerSeries)     (new-ISeriesBuilder len))
	       (else (error 'dest-mapping-series-builders
			    "Unknown series type ~a."
			    (SeriesDescription-type series))))))

(: key-cols-sort-lexical ((Listof Column) -> (Listof Column)))
(define (key-cols-sort-lexical cols)
  ((inst sort Column Column)
   cols
   (λ: ((kc1 : Column) (kc2 : Column))
       (string<=? (symbol->string (car kc1))
		  (symbol->string (car kc2))))))

(: key-cols-series ((Listof Column) -> (Listof IndexableSeries)))
(define (key-cols-series cols)
  (filter (λ: ((s : Series)) (or (CSeries? s)
				 (ISeries? s)))
	  (map column-series cols)))

;; Build key string from the columns of a frame and a given set of col labels to use.
;; Insert a tab char between each key value, e.g., k1 + \t + k2 + \t + ...

(: key-fn ((Listof IndexableSeries) -> (Index -> String)))
(define (key-fn cols)
  (let: ((col-refs : (Listof (Index -> (U Label Integer)))
		   (for/list ([col (in-list cols)])
			     (if (CSeries? col)
				 (cseries-referencer col)
				 (iseries-referencer col)))))
	(λ: ((row-id : Index))
	    (let ((outp (open-output-string)))
	      (for ([col-ref (in-list col-refs)])
		   (let*: ((seg : (U Symbol Integer) (col-ref row-id))
			   (seg-str : String (if (symbol? seg)
						 (symbol->string seg)
						 (number->string seg))))
			  (display seg-str outp)
			  (display key-delimiter outp)))
	      (get-output-string outp)))))

(: make-index (-> JoinHash))
(define (make-index)
  (make-hash))

(: index ((Listof IndexableSeries) -> JoinHash))
(define (index cols)

  (define: index : JoinHash (make-index))

  (define len (series-count (car cols)))
  (define: series-key : (Index -> String) (key-fn cols))

  (let loop ([i 0])
    (if (unsafe-fx>= i len)
	index
	(let: ((i : Index (assert i index?)))
	      (let ((key (series-key i)))
		(hash-update! index key
			      (λ: ((idx : (Listof Index)))
				  (cons i idx))
			      (λ () (list))))
	      (loop (add1 i))))))

(: cseries-copy-fn (CSeries CSeriesBuilder -> (Index -> Void)))
(define (cseries-copy-fn series builder)
  (let ((cseries-ref (cseries-referencer series)))
    (λ: ((i : Index))
	(append-CSeriesBuilder builder (cseries-ref i)))))

(: copy-column-row-error (Series Integer -> Void))
(define (copy-column-row-error series col)
  (error 'frame-merge "Invalid target builder for frame column series ~s at ~s"
	 (series-type series) col))

(: copy-column-row ((Vectorof Series) (Vectorof SeriesBuilder) Index -> Void))
(define (copy-column-row src-series dest-builders row-id)
;;  (when (zero? (modulo row-id 10000))
;;	(displayln (format "Copy row: ~a" row-id)))
  (for ([col (in-range (vector-length src-series))])
       (let ((series (vector-ref src-series col))
	     (builder (vector-ref dest-builders col)))
	 (cond
	  ((NSeries? series)
	   (let: ((num : Float (nseries-ref series row-id)))
		 (if (NSeriesBuilder? builder)
		     (append-NSeriesBuilder builder num)
		     (copy-column-row-error series col))))
	  ((CSeries? series)
	   (let: ((nom : Label (cseries-ref series row-id)))
		 (if (CSeriesBuilder? builder)
		     (append-CSeriesBuilder builder  nom)
		     (copy-column-row-error series col))))
	  ((ISeries? series)
	   (let: ((num : Fixnum (iseries-ref series row-id)))
		 (if (ISeriesBuilder? builder)
		     (append-ISeriesBuilder builder num)
		     (copy-column-row-error series col))))))))


(: do-join-build ((Vectorof Series) (Vectorof Series)
		  (Vectorof SeriesBuilder) (Vectorof SeriesBuilder)
		  (Index -> Key) JoinHash -> Void))
(define (do-join-build a-cols b-cols a-builders b-builders fa-key-fn join-hash)

  (define: a-col-cnt : Fixnum (vector-length a-cols))
  (define: b-col-cnt : Fixnum (vector-length b-cols))
  (define: fa-len    : Fixnum (series-count (vector-ref a-cols #{0 : Index} )))

  (for ((fa-row (in-range fa-len)))
       (let*: ((fa-row : Index (assert fa-row index?))
	       (fa-key : Key (fa-key-fn fa-row)))
	      (let ((fb-rows (hash-ref join-hash fa-key (λ () '()))))
		;; (displayln (format "Hash join: ~s ~s, ~s" fa-row fa-key fb-rows))
		(for ([fb-row fb-rows])
		     (copy-column-row a-cols a-builders fa-row)
		     (copy-column-row b-cols b-builders (assert fb-row index?)))))))

;; FIXME RPR - Currently only doing a left-outer join on fa to fb
;; Smart pick which to index, which to drive from sequentially, between Frame fa and Frame fb.
(: frame-merge (Frame Frame [#:on (Listof Symbol)] -> Frame))
(define (frame-merge fa fb #:on [cols '()])

  ;;  Directly using frame-explode with an internal define doesn't work.  TR BUG
  (: frame-cols (Frame LabelProjection -> (Listof Column)))
  (define (frame-cols frame project)
    (frame-explode frame #:project project))

  (: src-series ((Listof Column) -> (Vectorof Series)))
  (define (src-series cols)
    (list->vector (map column-series cols)))

  (define: cols-a    : (Setof Label) (list->set (frame-names fa)))
  (define: cols-b    : (Setof Label) (list->set (frame-names fb)))
  (define: join-cols : (Setof Label) (if (null? cols)
					 (set-intersect cols-a cols-b)
					 (set-intersect (list->set cols)
							(set-intersect cols-a cols-b))))

  (pretty-print cols-a)
  (pretty-print cols-b)
  (pretty-print join-cols)

  (define: non-key-common : (Setof Label) (set-subtract (set-intersect cols-a cols-b) join-cols))

  (pretty-print non-key-common)

  (when (null? join-cols)
	(error "No common columns between frames to join on."))

  (define: non-key-fb : (Setof Label) (set-subtract cols-b join-cols))

  (define: fa-cols : (Listof Column) (frame-cols fa '()))
  (define: fb-cols : (Listof Column) (frame-cols fb non-key-fb))

  (pretty-print fa-cols)
  (pretty-print fb-cols)

  (define: fb-index : JoinHash
    (let ((cols (key-cols-sort-lexical (frame-cols fb join-cols))))
      (index (key-cols-series cols))))

  (define: fa-keyfn : (Index -> Key)
    (key-fn (key-cols-series (key-cols-sort-lexical (frame-cols fa join-cols)))))

  (define: dest-builders-a : (Vectorof SeriesBuilder)
    (list->vector (dest-mapping-series-builders (frame-description fa) 10)))

  (define: dest-builders-b : (Vectorof SeriesBuilder)
    (list->vector
     (dest-mapping-series-builders (frame-description fb #:project non-key-fb) 10)))

  ;; side-effects into the builders
  (do-join-build (src-series fa-cols) (src-series fb-cols)
		 dest-builders-a dest-builders-b
		 fa-keyfn fb-index)

  (define: new-a-series : (Listof Column)
    (for/list ([builder (in-vector dest-builders-a)]
	       [col     (in-list fa-cols)])
	      (cons (join-column-name col non-key-common "fa-")
		    (series-complete builder))))

  (define: new-b-series : (Listof Column)
    (for/list ([builder (in-vector dest-builders-b)]
	       [col     (in-list fb-cols)])
	      (cons (join-column-name col non-key-common "fb-")
		    (series-complete builder))))

  (new-frame (append new-a-series new-b-series)))


;; Append common columns
(: frame-append (Frame Frame [#:col (Listof Symbol)] -> Frame))
(define (frame-append fa fb #:cols [cols '()])

  (define: cols-a    : (Setof Label) (list->set (frame-names fa)))
  (define: cols-b    : (Setof Label) (list->set (frame-names fb)))
  (define: append-cols : (Setof Label) (if (null? cols)
					   (set-intersect cols-a cols-b)
					   (set-intersect (list->set cols)
							  (set-intersect cols-a cols-b))))
  (new-frame (map (λ: ((name : Label))
		      (cons name (cseries-append (frame-cseries fa name)
						 (frame-cseries fb name))))
		  (filter (λ: ((name : Label))
			      (set-member? append-cols name))
			  (frame-names fa)))))
