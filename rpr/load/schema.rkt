#lang typed/racket/base

(provide
 alter-schema-columns
 alter-schema-no-headers
 generate-anon-series-names
 determine-Schema
 SeriesTypes
 (struct-out Schema)
 Schema-headers
 Schema-SeriesTypes
 (struct-out ColumnInfo))

(require 
 racket/pretty
 racket/set
 (only-in "parse.rkt"          
          parse-tab-line))

;; Schema definition of a data file. 
;; May be defined as meta information along with the data file
;; OR
;; Dynamically determined by sampling the data files.

(define-type SeriesTypes (U 'CATEGORICAL 'NUMERIC))

(struct: ColumnInfo ([name : Symbol]
                     [type : SeriesTypes]))

(struct: Schema ([has-headers : Boolean]
                 [meta : (Listof ColumnInfo)]))

(: Schema-headers (Schema -> (Listof Symbol)))
(define (Schema-headers schema)
  (map (λ: ((meta : ColumnInfo))
         (ColumnInfo-name meta))
       (Schema-meta schema)))

(: Schema-SeriesTypes (Schema -> (Listof SeriesTypes)))
(define (Schema-SeriesTypes schema)
  (map (λ: ((meta : ColumnInfo))
         (ColumnInfo-type meta))
       (Schema-meta schema)))

(: canonicalize-to-string-or-num ((Listof String) -> (Listof (U Number String))))
(define (canonicalize-to-string-or-num strs)
  (map (λ: ((str : String))
         (if (string=? (string-upcase str) "N/A")
             1
             (let ((num (string->number str)))
               (if num num str))))
       strs))

;; First line of sample is all strings (non-numeric) assume headers
(: guess-if-headers ((Listof String) -> Boolean))
(define (guess-if-headers fields)
  (andmap string? (canonicalize-to-string-or-num fields)))

(: guess-if-numeric ((Listof String) -> Boolean))
(define (guess-if-numeric col)
  (andmap number? (canonicalize-to-string-or-num col)))          

(: transpose-rows-to-cols ((Listof (Listof String)) -> (Listof (Listof String))))
(define (transpose-rows-to-cols rows)
  
  (: transpose-row-to-col ((Listof String) (Listof (Listof String)) -> (Listof (Listof String))))
  (define (transpose-row-to-col row cols)
    (let: loop : (Listof (Listof String)) ((row : (Listof String) row) 
                                           (cols : (Listof (Listof String)) cols) 
                                           (accum : (Listof (Listof String)) '()))
      (if (null? row)
          (reverse accum)
          (let ((elem (car row)) (col (car cols)))
            (loop (cdr row) (cdr cols) (cons (cons elem col) accum))))))
  
  (: create-accumulator ((Listof String) -> (Listof (Listof String))))
  (define (create-accumulator protos)
    (let: loop : (Listof (Listof String)) 
      ((protos : (Listof String) protos) 
       (accum : (Listof (Listof String)) '()))
      (if (null? protos)
          accum
          (loop (cdr protos) (cons '() accum)))))
  
  (: deep-reverse ((Listof (Listof String)) -> (Listof (Listof String))))
  (define (deep-reverse cols)
    (let: loop : (Listof (Listof String))
      ((cols : (Listof (Listof String)) cols)
       (accum : (Listof (Listof String)) '()))
      (if (null? cols)
          (reverse accum)
          (loop (cdr cols) (cons (reverse (car cols)) accum)))))
  
  (if (null? rows)
      '()
      (let: loop : (Listof (Listof String)) 
        ((rows : (Listof (Listof String)) rows)
         (cols : (Listof (Listof String)) (create-accumulator (car rows))))
        (if (null? rows)
            (deep-reverse cols)
            (loop (cdr rows) (transpose-row-to-col (car rows) cols))))))


(: generate-anon-series-names (Integer -> (Listof String)))
(define (generate-anon-series-names n)
  (define base "x")
  (for/list ([x (in-range n)])
    (string-append base (number->string x))))

(: guess-series-type ((Listof String) -> SeriesTypes))
(define (guess-series-type col)
  (if (guess-if-numeric col)
      'NUMERIC
      'CATEGORICAL))

(: guess-series-meta ((Listof String) (Listof (Listof String)) -> (Listof ColumnInfo)))
(define (guess-series-meta headers cols)
  (let: loop : (Listof ColumnInfo)
    ((headers : (Listof String) headers) 
     (cols : (Listof (Listof String)) cols) 
     (meta : (Listof ColumnInfo) '()))
    (if (null? headers)
        (reverse meta)
        (loop (cdr headers) 
              (cdr cols) 
              (cons (ColumnInfo (string->symbol(car headers))
                                (guess-series-type (car cols)))
                    meta)))))

(: determine-Schema ((Listof String) -> Schema))
(define (determine-Schema lines)
  
  (: sample-length ((Listof String) -> Integer))
  (define (sample-length sample)
    (length sample))
  
  (: check-consistent-fields ((Listof (Listof String)) -> Void))
  (define (check-consistent-fields samples)
    (when (pair? samples)
      (let ((l0 (sample-length (car samples))))              
        (unless (andmap (λ: ((sample : (Listof String))) 
                          (eq? (sample-length sample) l0))
                        samples)
          (error "Sampling of data file indicates lines of varying field length")))))
  
  (if (null? lines)
      (Schema #f '())
      (let ((samples (map parse-tab-line lines)))             
        (check-consistent-fields samples)
        (let ((headers? (guess-if-headers (car samples))))
          (let ((headers (if headers?
                             (car samples)                             
                             (generate-anon-series-names (length (car samples)))))
                (cols (if headers?
                          (transpose-rows-to-cols (cdr samples))  
                          (transpose-rows-to-cols samples))))            
            (Schema headers? (guess-series-meta headers cols)))))))

(: alter-schema-no-headers (Schema -> Schema))
(define (alter-schema-no-headers schema)
  (if (Schema-has-headers schema)
      (let* ((metas (Schema-meta schema))
             (hdrs (generate-anon-series-names (length metas))))
        (let: loop : Schema ((hdrs : (Listof Symbol) (map string->symbol hdrs))
                             (metas : (Listof ColumnInfo) metas) 
                             (accum : (Listof ColumnInfo) '()))
          (if (null? hdrs)
              (Schema #f (reverse accum))
              (loop (cdr hdrs) (cdr metas) 
                    (cons (ColumnInfo (car hdrs) (ColumnInfo-type (car metas)))
                          accum)))))
      schema))

(: alter-schema-columns (Schema (Listof (Pair Symbol ColumnInfo)) -> Schema))
(define (alter-schema-columns schema metas)
  
  (: new-cols (HashTable Symbol ColumnInfo))
  (define meta-map 
    (let ((hmap ((inst make-hash Symbol ColumnInfo))))
      (for-each (λ: ((meta : (Pair Symbol ColumnInfo)))
                  (hash-set! hmap (car meta) (cdr meta)))
                metas)
      hmap))
  
  (: replaced-metas (Listof ColumnInfo))
  (define replaced-metas (let: loop : (Listof ColumnInfo) 
                           ((old-metas : (Listof ColumnInfo) (Schema-meta schema))
                            (accum : (Listof ColumnInfo) '()))
                           (if (null? old-metas)
                               (reverse accum)
                               (let ((curr-meta (car old-metas)))			
                                 (loop (cdr old-metas)
                                       (cons
                                        (hash-ref meta-map 
                                                  (ColumnInfo-name curr-meta) 
                                                  (λ () curr-meta))
                                        accum))))))
  
  (struct-copy Schema schema [meta replaced-metas]))


