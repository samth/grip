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

#| Simple State Machines for Master Worker MapReduce nodes. |#

#lang typed/racket/base

(provide
 (struct-out MasterState)
 (struct-out WorkerState))

(provide:
 [worker-state (WorkerState -> WorkerStates)]
 [master-state (MasterState -> MasterStates)])

(require
 (only-in prelude/type/struct
	  serialize-struct-to-string))
 

#| Ready - Awaiting start of SWF event. |#
#| Scheduling - Determining and submitting to SWF all Map To Partition tasks |#
#| MPTracking - Tracking all Map To Partition tasks to completion |#
(define-type MasterStates (U 'Ready 'Scheduling 'MPTracking))

(define-type WorkerStates (U 'Ready))

(struct: MasterState ([state : MasterStates]) #:prefab)

(struct: WorkerState ([state : WorkerStates]) #:prefab)

(: master-sm (-> MasterState))
(define (master-sm)
  (MasterState 'Ready))

(: master-state (MasterState -> MasterStates))
(define (master-state ms)
  (MasterState-state ms))

(: new-master-state (MasterState MasterStates -> MasterState))
(define (new-master-state ms state)
  (MasterState state)) ;; yep I know.

(: worker-sm (-> WorkerState))
(define (worker-sm)
  (WorkerState 'Ready))

(: worker-state (WorkerState -> WorkerStates))
(define (worker-state ws)
  (WorkerState-state ws))

(: new-worker-state (WorkerState WorkerStates -> WorkerState))
(define (new-worker-state ws state)
  (WorkerState state))

(: serialize-master-state (MasterState -> String))
(define (serialize-master-state master-state)
  (serialize-struct-to-string master-state))
  
(: serialize-worker-state (WorkerState -> String))
(define (serialize-worker-state worker-state)
  (serialize-struct-to-string worker-state))
