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
 (struct-out MapDynFn1)
 map-reduce)

(require 
 (only-in racket/set
          list->set)
 (only-in racket/future
          processor-count)
 (only-in racket/place/distributed
          quote-module-path)
 (only-in venue/venue
          start-venue venue-event
          venue-channel-get venue-channel-put
          Venue)
 (only-in "logging.rkt"
          log-mr-info)
 (only-in "messages.rkt"
          MapTaskReqResp)
 (only-in "types.rkt"
          DynFn TextParse Write Map Group
          Block Block-name Block-range 
          Range-sod Range-eod 
          RDD RDD-blocksets))

(struct: (A B) MapDynFn1 ([module : Module-Path]
                          [fn     : Symbol]) #:transparent)                

(struct: Topology ([venues : (Listof Venue)]))

(struct: Task ([type : (U 'Map 'Reduce)]
               [blocks : (Listof Block)]))               

(struct: MapTask ([taskid : Natural]
                  [block  : Block]
                  [dynfn  : MapDynFn1])
	 #:transparent
	 #:methods gen:equal+hash
	 [(define (equal-proc mt1 mt2 rec?)
	    (equal? (MapTask-taskid mt1) (MapTask-taskid mt2)))
	  (define (hash-proc mt rec?)
	    (eqv-hash-code (MapTask-taskid mt)))
	  (define (hash2-proc mt rec?)
	    (eqv-hash-code (* 17 (MapTask-taskid mt))))])

(struct: TaskLedger ([issued : (Listof Task)]
                     [outstanding : (Listof Task)]
                     [completed : (Listof Task)]))

(: spawn-venues (case-> (-> (Listof Venue))
                        (Natural -> (Listof Venue))))
(define (spawn-venues [cnt 0])
  (let ((cpus (processor-count)))
    (for/list: : (Listof Venue) ([n (in-range (min cnt cpus))])
	       (let ((venue-name (string->symbol (string-append "MRSlave-" (number->string n)))))
		 (log-mr-info "Launching slave ~s" venue-name)
		 (start-venue venue-name
			      (string->path "/code/racketlib/mapred/slave.rkt")
			      'taskrunner-handler
			      #f 
			      (current-output-port)
			      (current-output-port))))))

(: make-map-request-msg (All (D) (Block D) -> MapTaskReqResp))
(define (make-map-request-msg block)
  (let ((range (Block-range block)))
    (let ((range (assert range)))
      (MapTaskReqResp ((inst Block-name Any) block)
		      (Range-sod range) 
		      (Range-eod range)))))

(: submit-map-task (Semaphore Venue MapDynFn1 Block -> Void))
(define (submit-map-task notify venue dynfn block)
  (thread (λ ()
	     (log-mr-info "Submit ~s for map and then partitioning on ~s" block venue)
	     (venue-channel-put venue (make-map-request-msg block))            
	     (let ((msg (sync (venue-event venue)))) ;; same as venue-channel-get              
	       (log-mr-info "Received ~s from ~s" msg venue)
	       (semaphore-post notify))))
  (void))

(: shuffle-rdd (All (A) (RDD A) -> (RDD A)))
(define (shuffle-rdd rddfile)
  rddfile)

(: reduce-rdd (All (A B) (RDD A) MapDynFn1 -> (RDD B)))
(define (reduce-rdd rddfile dynfn)
  rddfile)

(: map-rdd (All (A B) MapDynFn1 (RDD A) -> (RDD B)))
(define (map-rdd dynfn rddfile) 
  
  (log-mr-info "Performing map step on ~s with ~s" rddfile dynfn)
  
  
  ((inst RDD Void) '()))

(: map-reduce (All (A) (RDD A) TextParse Map Write Group -> Void))
(define (map-reduce rddfile parser mapper writer grouper)  
  
					;(define: tracker : (Tracker Block)
					;  (make-tracker (list->set (RDD-blocks rddfile))))
  
  (define: venues : (Listof Venue) (spawn-venues (processor-count)))
  
					;(let: ((rdd : (RDD A)   (map-rdd map-dynfn rddfile)))
					;  (let: ((rdd : (RDD A) (shuffle-rdd rdd)))
					;    (reduce-rdd rdd reduce-dynfn)
  (void))
