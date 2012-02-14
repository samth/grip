;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Knozama's Amazon API Library
;; Copyright (C) 2007,2008,2009,2010  Raymond Paul Racine
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
 ;;future-resource-xml
 fetch-resource-xml)

(require/typed (planet lizorkin/ssax:2:0/ssax)
	       (ssax:xml->sxml (Input-Port (Listof String) -> (List Any))))

(require
 (only-in "../uri.rkt"
	  Uri
	  Uri-authority Authority-host)
 (only-in "header.rkt"
	  Headers
	  agent-header
	  host-header)
 (only-in "http11.rkt"
	  http-invoke)
 (only-in "util.rkt"
	  ok-response?))

;; FIX ME RPR - This needs to be a 'generic' function in the sense
;; that the appropriate content handling fetch should be done 
;; depending on the HTTP content mime type,ie, if its XML content
;; fetch-resource-xml should be used.

;; Fetch of a XML resourse.
;; Returns SXML of fetched XML represented resource.
;; Note: Need to build up to a a synch. fetch-resource call
;; for a generic resource which returns the appropriate type
;; based on the mime type.
(: fetch-resource-xml (uri Headers -> (Option (Listof Any))))
(define (fetch-resource-xml url headers)
  (let ((auth (uri-authority url)))
    (if auth
	(let ((headers (append `(,(host-header (authority-host auth))
				 ,(agent-header "SOS/RL3/0.1")
				 "Accept: */*"))))
	  (let-values (((resp-data ip) (http-invoke 'GET url headers #f)))
	    (if (ok-response? resp-data)
		(let ((result (ssax:xml->sxml ip '())))
		  (close-input-port ip)
		  result)
		#f)))
	#f)))

;; return a resource via HTTP GET
;; returns the value of invoking error-proc on an error
;;(define future-resource-xml
;;  (lambda (url headers error-proc)
;;    (future-resource (lambda ()
;;		       (fetch-resource-xml url headers error-proc)))))

;; Fetches content into an ivar on its a dedicated task
;; returns: ivar
;;(define future-resource
;;  (lambda (fetch-thunk)
;;    (let ((ivar (make-ivar)))
;;      (task-start! (make-task (lambda ()
;;				(ivar-put ivar (fetch-thunk)))))
;;      ivar)))

