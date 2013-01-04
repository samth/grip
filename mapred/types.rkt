#lang typed/racket

(provide 
 (struct-out DynFn)
 Text Date Num Url
 Fields TextFields
 (struct-out Record)
 (struct-out Block) 
 (struct-out BlockSet)                   
 (struct-out Range)
 (struct-out RDDFile)
 TextRecord TextReader Transform
 Status (struct-out Success) (struct-out Failure) OK
 TextParser Writer Sorter Mapper Grouper Partitioner Partition
 BlockFormatter)

(require
 (only-in httpclient/uri
          Uri))

(define-type Status (U Success Failure))
(struct: Success ())
(struct: Failure ([msg : String]) #:transparent)
(define OK (Success))

(struct: DynFn ([module : Module-Path]
                [fn     : Symbol]) #:prefab)

;;; an RDD is a set of distributed blocks
(struct: (A) RDDFile ([blocksets : (Listof BlockSet)]) #:transparent)
  ;#:methods gen:custom-write [(define write-proc (λ (rddfile outp mode)
  ;                                                 (display (format "#<RDDFile blocks=~s>" 
  ;                                                                  (length (RDDFile-blocks rddfile))) outp)))])
;; Local Paths for now.
;; (url->path) (path->url)
(define-type Url Path)
  
(define-type Text String)
(define-type Date String)
(define-type Num Number)

(define-type Field (U Text Date Number))
(define-type Fields (Vectorof Field))
(define-type TextFields (Vectorof Text))

(struct: (K V) Record ([key : K] [value : V]) #:transparent)

(define-type TextRecord (Record String String))

(define-type TextReader (String -> TextRecord))

(define-type Transform (Record -> Record))

;; Get rid of this.
(define-type (BlockFormatter A) ((Block A) -> (Listof A)))

(define-type (TextParser D)  (Text -> D))
(define-type (Mapper D E)    (D -> E))
(define-type (Writer A)      (A Output-Port -> Void))
(define-type (Sorter A)      (A A -> Boolean))
(define-type (Partitioner A) (A -> Index))
(define-type (Partition D)   (Vectorof (RDDFile D)))
(define-type (Grouper D)     (D -> Index))

(struct: Range ([sod : Natural]
                [eod : Natural]) #:transparent)

;; Note the EOD marker is the exclusive marker of the position of the last record.
;; It may (most likely) indicates a position inside the last record.
;; Therefore a Reader of a Block should read past EOD until the record is complete and then no further.
;; As a corollary a block must extend past the EOD position at least 1 Record length in size.
;; This all works as a Reader reads the first complete record of a block and the last record which contains the EOD position.
;; All contigous blocks therefore must overlap by at least one MAX-RECORD-LENGTH.
;; All trouble is because we are assuming the persisted data API being used supports offset reading.
;; such as S3 and an OS.
;; A Block is a chunk of semi/structured data subject to manipulation
(struct: (D) Block ([name : String]
                    [range : Range]) #:transparent)
                
(struct: (D) BlockSet ([uri : Uri]
                       [blocks : (Listof Block)]) #:transparent)
