;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Ray Racine's Typed Racket Library
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

(provide define-tabbed-parser-for-layout)

(require/typed racket
               [string-trim (String -> String)])

(require
 (only-in "reader.rkt"
	  integer-field
	  float-field
	  string-field)
 (for-syntax 
  racket/pretty
  typed/racket/base  
  syntax/parse
  (only-in racket/syntax
           format-id)
  (only-in "../layout-types.rkt"
           Layout-name Layout-fields
           Field-name Field-type
           Field-offset Field-length)))

(begin-for-syntax 
  
  (: hash-fields (Layout -> (HashMap Symbol Field)))
  (define (hash-fields layout)
    (define: fmap : (HashMap Symbol Field) (make-hash))
    (for ([f (Layout-fields layout)])
      (hash-set! fmap (Field-name f) f))
    fmap)
  
  (: fields-to-project ((Listof Field) (Listof Syntax) -> (Listof Field)))
  (define (fields-to-project layout fields)
    (define field-dict (hash-fields layout))
    (for/list ([field fields])
      (let ((fname (syntax->datum field)))
        (hash-ref field-dict fname (λ () (error (format "Field `~a' is not defined in the layout" fname)))))))
  
  (: build-struct-field-syntax ((Listof Field) -> Syntax))
  (define (build-struct-field-syntax fields)
    (for/list ([field fields])
      #`[#,(Field-name field) : #,(Field-type field)]))
  
  (: build-parser-let-bindings ((Listof Field) -> Syntax))
  (define (build-parser-let-bindings fields)
    (for/list ([field fields])
      (let ((name (Field-name field))
            (type (Field-type field)))
	(case type
	  ((String)  #`(#,name : #,type (string-field inp)))
	  ((Integer) #`(#,name : #,type (integer-field inp)))
	  ((Float)   #`(#,name : #,type (float-field inp)))
	  ((Symbol)  #`(#,name : #,type (string->symbol (string-field inp))))	  
	  (else #`(#,name : #,type (string-field inp)))))))
  
  (: build-ctor-args ((Listof Field) -> Syntax))
  (define (build-ctor-args fields)
    (for/list ([field fields])
      #`#,(Field-name field)))
  
  (: extract-base-name (Syntax Symbol -> Syntax))
  (define (extract-base-name stx full-name)
    (define base (car (regexp-split #rx"-" (symbol->string full-name))))
    (datum->syntax stx (string->symbol base))))

(define-syntax (define-tabbed-parser-for-layout stx)    
  (syntax-parse stx
    [(_ (parser-name:id layout-name:id) (f0:id f1:id ...))
     (let* ((full-name (syntax-e #'layout-name))
            (base-name (extract-base-name stx full-name)))       
       (with-syntax ((desc-name (format-id #'layout-name "~a-desc" full-name))
                     (parser-struct (format-id #'layout-name "~a" base-name)))                 
         (let ((pfields (fields-to-project 
                         (syntax-local-value #'desc-name)
                         (syntax->list #'(f0 f1 ...)))))                          
           (with-syntax ((fields (build-struct-field-syntax pfields))
                         (bindings (build-parser-let-bindings pfields))
                         (args (build-ctor-args pfields)))             
             #`(begin
                 (struct: parser-struct fields)
                 (define:  parser-name : (String -> parser-struct)
                   (λ: ((inp : (U String Input-Port)))
		       (let: ((inp : Input-Port (if (string? inp) 
						    (open-input-string inp)
						    inp)))
			 (let: bindings
			   (parser-struct #,@#'args))))))))))]))
