;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Ray Racine's Munger Library
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

#| Calculate the map activity tasks, issue them, track their completion as a SWF Decider. |#

#lang typed/racket/base

(provide:
 [map-to-partition-decider (String -> Void)]
 [mapreduce-start (String MapReduceStart -> WorkflowExecution)]
 [start-mapwf-execution (String WorkflowType String -> WorkflowExecution)]
 [terminate-mapwf-execution (String WorkflowExecution [#:reason String] [#:details String]  -> Void)])

(require
 racket/pretty
 racket/match
 ;; (only-in racket/serialize DOESN'T WORK IN TR
 ;;	  deserialize)
 (only-in format/json/tjson
          JsObject)
 (only-in httpclient/uri/filescheme
	  local-file-uri->path
	  local-file-uri?)
 (only-in aws/swf/workflow
	  start-workflow-execution
	  terminate-workflow-execution)
 (only-in aws/swf/history
	  HistoryEvent HistoryEvent-event-type HistoryEvent-attributes)
 (only-in aws/swf/types
	  WorkflowType ActivityType
	  WorkflowExecution WorkflowExecution-workflow-id WorkflowExecution-run-id)
 (only-in aws/swf/task
	  ActivityTask 
	  DecisionTask DecisionTask-task-token
	  poll-for-decision-task)
 (only-in aws/swf/decision
	  fail-workflow-execution-decision
	  schedule-activity-task-decision
	  respond-decision-task-completed)
 (only-in mapred/rdd/rdd
          rdd-text)
 (only-in aws/dynamodb/parse
          attr-value-jsobject attr-value-jslist
          attr-value-integer attr-value-integer-opt
	  attr-value-string attr-value-string-opt)
 "../logging.rkt"
 (only-in "../types.rkt"
	  BlockSet
	  RDD RDD-blocksets)
 (only-in "../rdd/rdd.rkt"
	  n-rdd)
 (only-in "../messages.rkt"
	  MapReduceStart MapReduceStart-path MapReduceStart-split-size MapReduceStart-task-size
	  serialize-MapReduceStart-msg deserialize-MapReduceStart-msg))

(define MR-TASK-LIST "mr-master")
(define MR-MAP-WORKFLOW-TYPE (WorkflowType "mapreduce" "1.0"))

(: mapreduce-start (String MapReduceStart -> WorkflowExecution))
(define (mapreduce-start domain start-msg)  
  (match start-msg
    ((MapReduceStart input-path split-size task-size)
     (log-mr-info "Starting new mapreduce workflow ~s" MR-MAP-WORKFLOW-TYPE)
     (if (local-file-uri? input-path)
	 (start-mapwf-execution domain MR-MAP-WORKFLOW-TYPE (serialize-MapReduceStart-msg start-msg))
	 (error "S3 input directories not supported yet")))))

(: start-mapwf-execution (String WorkflowType String -> WorkflowExecution))
(define (start-mapwf-execution domain workflow-type input)
  (log-mr-info "Starting MR-Mapping workflow ~s in ~s" domain workflow-type)
  (start-workflow-execution domain MR-MAP-WORKFLOW-TYPE #:input input  #:task-timeout (* 3 60)))

(: terminate-mapwf-execution (String WorkflowExecution [#:reason String] [#:details String]  -> Void))
(define (terminate-mapwf-execution domain wf-run #:reason [reason ""] #:details [details ""])
  (pretty-print (WorkflowExecution-workflow-id wf-run))
  (pretty-print (WorkflowExecution-run-id wf-run))
  (terminate-workflow-execution domain 
				(WorkflowExecution-workflow-id wf-run)
				#:run-id (WorkflowExecution-run-id wf-run)
				#:reason reason
				#:details details))


(: serialize-blockset (BlockSet -> String))
(define (serialize-blockset blockset)
  (let ((sout (open-output-string)))
    (write blockset sout)
    (get-output-string sout)))

;; Process a MR workflow start.
(: schedule-map-to-partition-activity-decision (BlockSet -> JsObject))
(define (schedule-map-to-partition-activity-decision blockset)
  (schedule-activity-task-decision (ActivityType "mr-map" "1.0")
				   #:input (serialize-blockset blockset)
				   #:schedule-to-start-timeout (* 3 60)))

(: read-mapreduce-start-message (String -> MapReduceStart))
(define (read-mapreduce-start-message msg-str)
  (let ((sin (open-input-string msg-str)))
    (cast (read sin) MapReduceStart)))

(: decision-for-started-event (DecisionTask HistoryEvent -> Void))
(define (decision-for-started-event decision event)
  (if (not (eq? (HistoryEvent-event-type event)
		'WorkflowExecutionStarted))
      (respond-decision-task-completed (DecisionTask-task-token decision)
				       "context here"
				       (list (fail-workflow-execution-decision "Unexpected HistoryEvent"
									       "Expected to find WorkflowExecutionStarted")))
      (let* ((attrs (HistoryEvent-attributes event))
	     (input (attr-value-string attrs 'input)))
	(match (read-mapreduce-start-message input)
	  [(and msg (MapReduceStart path split-size task-size))
	   (let ((local-path (local-file-uri->path path)))
	     (log-mr-info "Splitting ~s into blocks." local-path)
	     (let ((rdd (n-rdd (rdd-text local-path split-size) task-size)))
	       (pretty-print rdd)
	       (respond-decision-task-completed (DecisionTask-task-token decision)
						"context here"
						(map (λ: ((blockset : BlockSet)) 
							 (schedule-map-to-partition-activity-decision blockset))
						     (RDD-blocksets rdd)))))]))))

(: map-to-partition-decider (String -> Void))
(define (map-to-partition-decider domain)
  (log-mr-info "Master polling for decision task")
  (let ((task (poll-for-decision-task domain MR-TASK-LIST)))
    (displayln "=============================================")
    (displayln "=============================================")
    (pretty-print task)
    (displayln "=============================================")
    (displayln "=============================================")
    (match task
      ((and decision-task (DecisionTask task-token events next-page-token started-event-id previous-started-event-id workflow-execution workflow-type))
       (let ((history (list->vector events)))
	 (if (zero? previous-started-event-id)
	     (begin
	       (log-mr-info "Processing ExecutionStarted")
	       (decision-for-started-event decision-task (vector-ref history 0)))	  
	     (log-mr-info "FIX ME Not handling decision task beyond started event."))))
      (else (if task
		(log-mr-error "Decision Task was not handled.")
		(log-mr-info "No pending decision activity - timeout"))))))
