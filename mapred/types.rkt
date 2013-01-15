;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Ray Racine's Munger API Library
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

#lang typed/racket

(provide 
 (struct-out S3Partition)
 (struct-out DynFn)
 Text Date Num Url
 Fields TextFields
 (struct-out Record)
 (struct-out Block) 
 (struct-out BlockSet)                   
 (struct-out Range)
 (struct-out RDD)
 GroupCompare
 TextRecord TextReader Transform
 Status (struct-out Success) (struct-out Failure) OK
 TextParse Write Sort Group 
 Partition ;Partition
 Map Reduce
 BlockFormat)

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
(struct: (A) RDD ([blocksets : (Listof BlockSet)]) #:transparent)
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

(define-type (BlockFormat A) (Uri (Block A) -> (Listof A)))

(define-type (GroupCompare D) (D D -> Boolean)) ;; to do D's have the same key
(define-type (TextParse D) (Text -> D))
(define-type (Map D E) (D -> E))
(define-type (Reduce A B) ((Listof A) -> B))
(define-type (Write A) (A Output-Port -> Void))
(define-type (Sort A) (A A -> Boolean))
(define-type (Partition A) (A -> Index))
;; (define-type (Partition D)   (Vectorof (RDDFile D)))
(define-type (Group D) (D -> Index))

(struct: S3Partition ([bucket : String]
		      [job-id  : Natural]
		      [partition-id   : Natural]))		       

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
                    [range : (Option Range)]) #:transparent)
                
(struct: (D) BlockSet ([uri : Uri]
                       [blocks : (Listof Block)]) #:transparent)
