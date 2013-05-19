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

#| Handle mapping source data blocks to partition data blocks via SWF activity task requests. |# 

#lang typed/racket/base

(provide 
 map-to-partition-worker)

(require 
 racket/pretty
 racket/match
 (only-in "../logging.rkt"
	  log-mr-info)
 (only-in aws/swf/task
	  ActivityTask
	  poll-for-activity-task)
 (only-in aws/swf/activity
	  respond-activity-task-completed))

(define MR-SLAVE "mr-slave")

(: map-to-partition-worker (String String -> Void))
(define (map-to-partition-worker domain identity)
  (log-mr-info "MR activity poll for ~s by ~s queue ~s" domain identity MR-SLAVE)
  (let ((task (poll-for-activity-task domain identity MR-SLAVE)))
    (log-mr-info "Received activity task.  Worker: ~s" identity)
    (match task
      ((ActivityTask id type input started-event token workflow)
       (log-mr-info "Processing Activity task ~s ~s task with input: ~s" id type input)
       (respond-activity-task-completed token "Oky Doky"))
      (else (if task
		(begin
		  (log-mr-info "Unhandled activity task.  Worker: ~s" identity)
		  (pretty-print task))
		(log-mr-info "Task poll timeout. Worker: ~s" identity))))))

    
    
    
    
    
				      
