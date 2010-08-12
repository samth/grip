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

#lang racket

(provide
 ;;future-resource-xml
 fetch-resource-xml)

(require
 (planet lizorkin/ssax:2:0/ssax)
 (only-in "../uri.rkt"
	  uri-authority authority-host)
 (only-in "headers.rkt"
	  agent-header
	  host-header)
 (only-in "http11.rkt"
	  parse-http-response-line
	  response-line-code
	  http-invoke))

;; FIX ME RPR - This needs to be a 'generic' function in the sense
;; that the appropriate content handling fetch should be done 
;; depending on the HTTP content mime type,ie, if its XML content
;; fetch-resource-xml should be used.

;; Fetch of a XML resourse.
;; Returns SXML of fetched XML represented resource.
;; Note: Need to build up to a a synch. fetch-resource call
;; for a generic resource which returns the appropriate type
;; based on the mime type.
(define fetch-resource-xml
  (lambda (url headers error-proc)    
    (let ((headers (append `(,(host-header (authority-host (uri-authority url)))
			   ,(agent-header "SOS/RL3/0.1")
			   "Accept: */*"))))
      (let-values (((resp-data ip) (http-invoke 'GET url headers #f)))
	(let ((result (let ((http-resp (parse-http-response-line (car resp-data))))
		      (if (string=? (response-line-code http-resp) "200")
			 (ssax:xml->sxml ip '())
			 (error-proc)))))
	  (close-input-port ip)
	  result)))))

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

