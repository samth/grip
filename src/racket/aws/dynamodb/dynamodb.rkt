#lang typed/racket/base

(provide
 create-table
 Key Key? Key-name Key-type
 Throughput Throughput? Throughput-read Throughput-write)

(require 
 (only-in "createtable.rkt"
	  Key Key? Key-name Key-type
	  Throughput Throughput? Throughput-read Throughput-write
	  create-table))
