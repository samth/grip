;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Bravais' Edito Princeps: EBook Tool Suite	    
;; Copyright (C) 2007  Raymond Paul Racine
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

(library 
 (aws awscredentials)
 
 (export
  aws-credentials-account-id
  aws-credentials-access-key
  aws-credentials-secret-key
  load-credentials)
 
 (import
  (rnrs base)
  (only (rnrs lists)
        assoc)
  (only (rnrs io simple)
        read call-with-input-file) 
  (err5rs records syntactic))
 
 (define-record-type aws-credentials #t #t account-id access-key secret-key)
 
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ;; Read in aws properties from a protected file.
 ;; string? -> aws-credentials?
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 (define (load-credentials fpath)
   (call-with-input-file fpath
     (lambda (ip)
       (let ((props (read ip)))
         (make-aws-credentials (cdr (assoc 'account-id props))
                               (cdr (assoc 'access-key props))
                               (cdr (assoc 'secret-key props)))))))
 )