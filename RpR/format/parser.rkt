#lang typed/racket/base

(provide define-parser-for-layout)

(require
 (for-syntax 
  racket/pretty
  typed/racket/base
  syntax/parse
  (only-in racket/syntax
           format-id)
  (only-in "layout-types.rkt"
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
                (type (Field-type field))
                (start (Field-offset field)))
            (let ((end (sub1 (+ start (Field-length field)))))    
              #`(#,name : #,type (substring line #,start #,end))))))
  
  (: build-ctor-args ((Listof Field) -> Syntax))
  (define (build-ctor-args fields)
    (for/list ([field fields])
      #`#,(Field-name field)))
  
  )
    
(define-syntax (define-parser-for-layout stx)    
  (syntax-parse stx
    [(_ name:id (f0:id f1:id ...))
     (let ((sname (syntax-e #'name)))
       (with-syntax ((desc-name (format-id #'name "~a-desc" sname))
                     (parser-name (format-id #'name "~a-parser" sname))
                     (parser-struct (format-id #'name "~a-fields" sname)))
         (let ((pfields (fields-to-project 
                         (syntax-local-value #'desc-name)
                         (syntax->list #'(f0 f1 ...)))))                          
           (with-syntax ((fields (build-struct-field-syntax pfields))
                         (bindings (build-parser-let-bindings pfields))
                         (args (build-ctor-args pfields)))             
             #`(begin
                 (struct: parser-struct fields #:transparent)
                 (define:  parser-name : (String -> parser-struct)
                   (λ: ((line : String))
                     (let: bindings
                       (parser-struct #,@#'args)))))))))]))