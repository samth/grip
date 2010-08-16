;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#| HTTP Cookies |#

#lang racket/base

(provide  make-cookie
	  parse-cookie)

(require racket/date)

(define make-cookie
  (lambda (domain path name value expire-secs)    
    (let ((duration (+ (current-seconds) expire-secs)))
      (date-display-format 'rfc2822)
      (let ((expire-date (date->string (seconds->date duration))))
	(string-append name "=" value "; "
		       "Expires=" expire-date "; "
		       "Path="    path "; "
		       "Domain="  domain)))))


(define parse-cookie
  (lambda (cookie)
    (if (string? cookie)
       (let ((is (open-input-string cookie))
	   (os (open-output-string)))
	 (let loop ((avs '()) (ws #f) (attr #f))
	   (let ((ch (read-char is)))
	     (cond 
	      ((eof-object? ch)
	       (if attr
		  (cons (cons attr (get-output-string os)) avs)
		  avs))
	      ((and (not ws) (char=? ch #\space))
	       (loop avs ws attr))
	      (else
	       (if attr
		  (if (char=? ch #\;)
		     (let ((value (get-output-string os #t)))
		       (loop (cons (cons attr value) avs) #f #f))
		     (let ((ws (if (char=? ch #\")
				(not ws)
				#t)))
		       (write-char os ch)
		       (loop avs ws attr)))
		  (if (char=? ch #\=)
		     (let ((attr (get-output-string os #t)))
		       (loop avs ws attr))
		     (begin
		       (write-char os ch)
		       (loop avs ws attr)))))))))
       '())))


