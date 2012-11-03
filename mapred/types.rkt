#lang typed/racket

(provide 
 Text Date Num Url
 Fields TextFields
 (struct-out Record)
 TextRecord TextReader Transform
 Status (struct-out Success) (struct-out Failure) OK)

(define-type Status (U Success Failure))
(struct: Success ())
(struct: Failure ([msg : String]) #:transparent)
(define OK (Success))

;; Local Paths for now.
;; (url->path) (path->url)
(define-type Url Path)
  
(define-type Text String)
(define-type Date String)
(define-type Num Number)

(define-type Field (U Text Date Number))
(define-type Fields (Vectorof Field))
(define-type TextFields (Vectorof Text))

(struct: (K V) Record ([key : K] [value : V]) #:transparent)

(define-type TextRecord (Record String String))

(define-type TextReader (String -> TextRecord))

(define-type Transform (Record -> Record))
