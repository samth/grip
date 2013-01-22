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

#| Spawn Local or Remote Nodes Operating As Slaves Or Master |#

#lang typed/racket/base

(provide:
 [spawn-venues (case-> (-> (Listof Venue))
		       (Natural -> (Listof Venue)))])

(require
"logging.rkt"
(only-in racket/future
	 processor-count)
(only-in venue/venue
	 start-venue venue-event
	 venue-channel-get venue-channel-put
	 Venue))

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
