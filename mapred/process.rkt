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

#| Handle master and slave process, venue the thread handling |#

#lang typed/racket/base

(provide
 thread-launch-mp-worker
 thread-launch-master-decider)

(require 
 (only-in httpclient/uri/guid
	  guid)
 "logging.rkt"
 (only-in "workflow/mpwf-worker.rkt"
	  map-to-partition-worker)
 (only-in "workflow/mpwf-master.rkt"
	  map-to-partition-decider))

(: thread-launch-mp-worker (String -> Thread))
(define (thread-launch-mp-worker domain)
  (define identity (guid))
  (log-mr-info "Launching map to partition worker.")
  (thread (λ ()
	     (let loop ()
	       (map-to-partition-worker domain identity)
	       (loop)))))

(: thread-launch-master-decider (String -> Thread))
(define (thread-launch-master-decider domain)
  (log-mr-info "Launching MapReduce Master Decider")
  (thread (λ ()
	     (let loop ()
	       (map-to-partition-decider domain)
	       (loop)))))
