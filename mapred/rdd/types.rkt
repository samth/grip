#lang typed/racket/base

(provide
 Location
 (struct-out BlockStructure)
 (struct-out Block))
 
;; The location of a block.
;; An absolute location will cluster globally uniqually identify the location of a block.
;; A partial location must be capable of being resolved to an absolution location.
(define-type Location Path)

;; Note the EOD marker is the inclusive marker of the position of the last record.
;; It may (most likely) indicates a position inside the last record.
;; Therefore a Reader of a Block should read past EOD until the record is complete and then no further.
;; As a corollary a block must extend past the EOD position at least 1 Record length in size.
;; This all works as a Reader reads the first complete record of a block and the last record which contains the EOD position.
;; All contigous blocks therefore must overlap by at least one MAX-RECORD-LENGTH.
;; All trouble is because we are assuming the persisted data API being used supports offset reading.
;; such as S3 and an OS.
(struct: BlockStructure ([offset : Natural]
                         [eod : Natural])) ;; Where the data ends offset - eod = block-size

;; A Block is a chunk of data subject to manipulation
(struct: Block ([loc : Location]
                [structure : BlockStructure]) #:transparent)
