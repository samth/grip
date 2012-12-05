#lang typed/racket/base

(provide 
 define-fixed-layout)

(require  
 (only-in "layout-types.rkt" Field Layout)
 (for-syntax 
  syntax/parse
  typed/racket
  (only-in racket/syntax
           format-id)
  (only-in "layout-types.rkt" Field Layout)))

(define-syntax (define-fixed-layout stx)
  
  (define-syntax-class field
    (pattern (fid:id type:id length:nat)))
  
  (: offset : Natural)
  (define offset 0)
  
  (: field (Listof Field))
  (define fields '())
  
  (: tr-type-for-field-type (Char -> Symbol))
  (define (tr-type-for-field-type ftype)
    (case ftype
      ((I) 'Integer)
      ((C) 'String)
      ((N) 'Number)
      ((D) 'String)
      ((S) 'Symbol)
      (else 'Nothing)))
  
  (define (field-syntax-with-offsets fs)        
    (for/list ([f fs])
      (syntax-parse f
        ((fid:id ftype:id flen:nat)
         (let ((curr-offset offset))
           (with-syntax ((tr-type (tr-type-for-field-type (syntax->datum #'ftype)))
                         (foffset curr-offset)
                         (fname (syntax->datum #'fid)))
             (set! fields (cons (Field (syntax->datum #'fid)
                                       'String
                                       curr-offset
                                       (syntax->datum #'flen))
                                fields))
             (set! offset (+ offset (syntax->datum #'flen)))       
             #`(Field 'fname 'tr-type foffset flen)))))))
  
  (syntax-parse stx
    [(_ name:id f0:field f1:field ...)   
     (with-syntax ((lo  #`(list #,@(field-syntax-with-offsets 
                                    (syntax->list #'(f0 f1 ...)))))
                   (desc-name (format-id #'name "~a-desc" (syntax-e #'name))))       
       (let ((name-id (symbol->string (syntax->datum #'name))))
         #'(begin
             (define-syntax desc-name (Layout 'name lo))
             (define name (Layout 'name lo)))))]))

