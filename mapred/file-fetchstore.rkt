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

#lang typed/racket/base

(provide:
 [file-blockset-iteratee (All (D) (Write D) Path String -> (Iteratee D (BlockSet D)))])

(require
 (only-in httpclient/uri
	  Uri)
 (only-in httpclient/uri/filescheme
	  local-path->uri)
 (only-in iteratee
	  icomplete Iteratee Continue Done Stream)
 (only-in "types.rkt" 
	  Range Block BlockSet Write))

(: file-blockset-iteratee (All (D) (Write D) Path String -> (Iteratee D (BlockSet D))))
(define (file-blockset-iteratee writer path fname)
  
  (define file-path (build-path path fname))
  (define: pout : Output-Port (open-output-file file-path))
  
  (: step ((Stream D) -> (Iteratee D (BlockSet D))))
  (define step
    (λ: ((s : (Stream D)))
	(cond
	 ([eq? s 'Nothing] 
	  (Continue step))
	 ([eq? s 'EOS]
	  (close-output-port pout)
	  (Done 'EOS  (BlockSet (local-path->uri path) 
				(list (Block fname (Range 0 (file-size file-path)))))))
	 (else (begin
		 (writer s pout)
		 (Continue step))))))
  
  (Continue step))
