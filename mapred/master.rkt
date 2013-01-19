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
 thread-launch-master-decider)

(require 
 "logging.rkt"
 (only-in racket/set
          list->set)
 (only-in racket/place/distributed
          quote-module-path)
 (only-in "logging.rkt"
          log-mr-info)
 (only-in "types.rkt"
          DynFn TextParse Write Map Group
          Block Block-name Block-range 
          Range-sod Range-eod 
          RDD RDD-blocksets)
 (only-in "workflow/mapwf.rkt"
	  map-to-partition-decider
	  mapreduce-start))
 

(: thread-launch-master-decider (String -> Thread))
(define (thread-launch-master-decider domain)
  (thread (λ ()
	     (let loop ()
	       (map-to-partition-decider domain)
	       (loop)))))

;; (struct: (A B) MapDynFn1 ([module : Module-Path]
;;                           [fn     : Symbol]) #:transparent)                

;; (struct: Task ([type : (U 'Map 'Reduce)]
;;                [blocks : (Listof Block)]))               

;; (struct: MapTask ([taskid : Natural]
;;                   [block  : Block]
;;                   [dynfn  : MapDynFn1])
;; 	 #:transparent
;; 	 #:methods gen:equal+hash
;; 	 [(define (equal-proc mt1 mt2 rec?)
;; 	    (equal? (MapTask-taskid mt1) (MapTask-taskid mt2)))
;; 	  (define (hash-proc mt rec?)
;; 	    (eqv-hash-code (MapTask-taskid mt)))
;; 	  (define (hash2-proc mt rec?)
;; 	    (eqv-hash-code (* 17 (MapTask-taskid mt))))])

