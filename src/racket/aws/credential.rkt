;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Knozama's Amazon API Library
;; Copyright (C) 2007-2011  Raymond Paul Racine
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

;; Load AWS credentials from a protected file.

#lang typed/racket/base

(provide
 current-aws-credential
 set-aws-credential!
 init-aws-credential
 Aws-Credential
 Aws-Credential-account-id
 Aws-Credential-access-key
 Aws-Credential-secret-key
 Aws-Credential-associate-tag)

(require/typed racket/base
	       ((read read-creds) (Input-Port -> (Listof (Pair Symbol String)))))
 
(require 
 (only-in (planet knozama/common:1/std/opt)
	  opt-apply-orelse))

(struct: Aws-Credential ((account-id : String)
			 (access-key : String)
			 (secret-key : String)
			 (associate-tag : String)))

(: default-cred-path Path)
(define default-cred-path
  (build-path (find-system-path 'home-dir) ".awscreds.sexp"))

(: current-aws-credential (Parameterof Aws-Credential))
(define current-aws-credential (make-parameter (Aws-Credential "" "" "" "")))

(: set-aws-credential! (Aws-Credential -> Void))
(define (set-aws-credential! cred)
  (set! current-aws-credential (make-parameter cred)))

(: init-aws-credential (Path -> Void))
(define (init-aws-credential path)
  (set-aws-credential! (load-credential path)))

(: load-credential (Path -> Aws-Credential))
(define (load-credential fpath)
  
  (define lookup (inst assoc Symbol String))
  (define value  (inst cdr Symbol String))

  (: cred-value (Symbol (Listof (Pair Symbol String)) -> String))
  (define (cred-value sym props)
    (opt-apply-orelse  (lookup sym props) value  ""))
  
  (call-with-input-file fpath
    (lambda: ((ip : Input-Port))
      (let: ((props : (Listof (Pair Symbol String))(read-creds ip)))
	(Aws-Credential (cred-value 'account-id props)
			(cred-value 'access-key props)
			(cred-value 'secret-key props)
			(cred-value 'associate-tag props))))))

(init-aws-credential default-cred-path)
