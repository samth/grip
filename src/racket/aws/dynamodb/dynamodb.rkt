;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Knozama's Amazon API Library
;; Copyright (C) 2012  Raymond Paul Racine
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
 create-table CreateTableResp 
 delete-table describe-table
 get-item 
 put-item PutItemResp
 ReturnValues
 list-tables ListTablesResp 
 Key Key? Key-name Key-type
 KeyVal KeyVal?
 ItemKey ItemKey?
 Item Item?
 Throughput Throughput? Throughput-read Throughput-write)

(require
 (only-in "types.rkt"
	  Throughput Throughput? Throughput-read Throughput-write
	  Key Key? Key-name Key-type
	  KeyVal KeyVal?
	  Exists Exists? ReturnValues
	  ItemKey ItemKey?
	  Item Item? Item-name Item-type Item-value)
 (only-in "createtable.rkt"
	  create-table CreateTableResp)
 (only-in "deletetable.rkt"
	  delete-table DeleteTableResp)
 (only-in "describetable.rkt"
	  describe-table DescribeTableResp DescribeTableResp?)
 (only-in "listtable.rkt"
	  list-tables ListTablesResp ListTablesResp?)
 (only-in "getitem.rkt"
	  get-item)
 (only-in "putitem.rkt"
	  put-item
	  PutItemResp PutItemResp?))
