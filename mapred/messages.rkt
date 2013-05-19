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

#lang typed/racket/base

(provide
 (struct-out MapReduceStart) serialize-MapReduceStart-msg deserialize-MapReduceStart-msg
 (struct-out MapToPartitionActivity))

(require
 (only-in prelude/type/struct
	  serialize-struct-to-string
	  deserialize-struct-from-string)
 (only-in httpclient/uri
          Uri parse-uri extend-path)
 (only-in "types.rkt"
          DynFn Block BlockSet
          Map Write Sort Group))

(struct: TaskMsg () #:prefab)

(struct: S3SplitMsgReq TaskMsg ([bucket : String] ;; S3 bucket
				[prefix : String] ;; prefix for S3 object listing
				[marker : String] ;; marker to commence S3 listing from
				[min-splits : Natural] ;; min number of splits to create if suffient S3 objects
				[max-split-size : Natural]) ; Max size in bytes of a split
	 #:prefab)

(struct: S3SplitMsgResp TaskMsg ([blockset : BlockSet] ;; Splits of S3 objects as a BlockSet
				 [marker : String])    ;; marker of last S3 object split
	 #:prefab)

(struct: MapTaskReqResp TaskMsg ([loc : String]
                                 [sod : Natural]
                                 [eod : Natural]) #:prefab)

(struct: MapReduceStart 
	 ([path : Uri]            ; Path to source files, local directory, S3 prefix, etc.
	  [split-size : Natural]  ; Max size in bytes to chop large files into
	  [task-size  : Natural]) ; Size of Blocksets assigned as atomic map to partition work units.
	 #:prefab)

(struct: MapToPartitionActivity
	 ([src-blockset : BlockSet]
	  [out-path     : Path]
	  [partitions   : (Listof String)])
	 #:prefab)

;; (struct: (A B) MRInit TaskMsg ([parser  : DynFn]
;;                                [mapper  : DynFn]
;;                                [writer  : DynFn]
;;                                [sorter  : DynFn]
;;                                [grouper : DynFn]
;;                                [partitions : Index]) #:prefab)

(: serialize-MapReduceStart-msg (MapReduceStart -> String))
(define (serialize-MapReduceStart-msg start-msg)
  (serialize-struct-to-string start-msg)) ;; RPR FixMe - What is proper "serialization" method in TR? prefab??

(: deserialize-MapReduceStart-msg (String -> MapReduceStart))
(define (deserialize-MapReduceStart-msg msg-str)
  (deserialize-struct-from-string msg-str MapReduceStart))

