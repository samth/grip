;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Ray Racine's TR Library
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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#lang typed/racket/base

(provide:
 [named-interfaces (-> (Listof String))]
 [interface-mac-address (String -> String)])

(require 
 (only-in "filepath.rkt"
	  FilePath RelFilePath AbsFilePath 
	  FilePath->string
	  make-AbsFilePath make-RelFilePath build*))

#| Constrains us to a standard Linux. 
MAC addresses found at:
"/sys/class/net/eth0/address")
"/sys/class/net/wlan0/address"
etc. |#

(: base-path-to-mac AbsFilePath)
(define base-path-to-mac (make-AbsFilePath "/sys/class/net"))

(: mac-file RelFilePath)
(define mac-file (make-RelFilePath "address"))

(: interface-path (String -> FilePath))
(define (interface-path iname)
  (build* base-path-to-mac (list  (make-RelFilePath iname) mac-file)))

(: filter-colons (String -> String))
(define (filter-colons mac)
  (regexp-replace* #rx":" mac ""))

(: named-interfaces (-> (Listof String)))
(define (named-interfaces)
  (map path->string (directory-list (FilePath->string base-path-to-mac))))

(: interface-mac-address (String -> String))
(define (interface-mac-address name)  
  (let ((mac-path (FilePath->string (interface-path name))))
    (if (file-exists? mac-path)
	(let ((mac-addr (with-input-from-file mac-path
			  read-line
			  #:mode 'text)))
	  (if (string? mac-addr)
	      (filter-colons mac-addr)
	      (error 'interface-mac-address 
		     "Failed to read MAC address from ~s." 
		     mac-path)))
	(error 'interface-mac-address "Unknown interface ~s" name))))
