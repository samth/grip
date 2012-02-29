;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Knozama's Amazon API Library
;; Copyright (C) 2012  Raymond Paul Racine
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
 load-key)

(require/typed racket/base
	       ((read read-creds) (Input-Port -> (Listof (Pair Symbol String)))))

(require
 (only-in (planet rpr/prelude:1/std/opt)
	  opt-apply-orelse))

(: default-access-config Path)
(define default-access-config
  (build-path (find-system-path 'home-dir)
	      ".google-api.sexp"))

(: load-key ((Option Path) -> String))
(define (load-key path)
  
  (define lookup (inst assoc Symbol String))
  (define value  (inst cdr Symbol String))
  
  (: cred-value (Symbol (Listof (Pair Symbol String)) -> String))
  (define (cred-value sym props)
    (opt-apply-orelse  (lookup sym props) value  ""))
  
  (let ((path (if path path default-access-config)))
    (call-with-input-file path
      (lambda: ((ip : Input-Port))
	(let: ((props : (Listof (Pair Symbol String)) (read-creds ip)))
	  (cred-value 'access-key props))))))
