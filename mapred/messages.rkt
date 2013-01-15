;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Ray Racine's MapReduce API Library
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
 (struct-out MapReduceStart)
 (struct-out MRInit)
 (struct-out StartMapPhase)
 (struct-out StartReducePhase)
 (struct-out TaskMsg)
 (struct-out MapTaskReqResp))

(require
 (only-in httpclient/uri
          Uri parse-uri extend-path)
 (only-in "types.rkt"
          DynFn Block
          Map Write Sort Group))

(struct: TaskMsg () #:prefab)

(struct: MapTaskReqResp TaskMsg ([loc : String]
                                 [sod : Natural]
                                 [eod : Natural]) #:prefab)

(struct: BlockMap ([src : Block]
                   [dest : (Listof Block)]))


(struct: MapReduceStart 
	 ([path : Uri] ; Path to source files, local directory, S3 prefix, etc.
	  [split-size : Natural] ; Max size in bytes to chop large files into
	  [task-size  : Natural] ; Size of Blocksets assigned as atomic map to partition work units.
	  ) #:prefab)

(struct: (A B) MRInit TaskMsg ([parser  : DynFn]
                               [mapper  : DynFn]
                               [writer  : DynFn]
                               [sorter  : DynFn]
                               [grouper : DynFn]
                               [partitions : Index]) #:prefab)

(struct: StartMapPhase    TaskMsg ([partition-count : Index]) #:prefab)
(struct: StartReducePhase TaskMsg () #:prefab)

