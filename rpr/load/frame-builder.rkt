#lang typed/racket/base

(provide 
 (struct-out FrameBuilder)
 append-data-fields)

(require 
 (only-in "../frame/series-builder.rkt"
          SeriesBuilder))

(struct: FrameBuilder ([builders : (Listof SeriesBuilder)]))

(: check-all-data-processed ((Listof Any) (Listof Any) -> Boolean))
(define (check-all-data-processed l1 l2)
  (and (null? l1) (null? l2)))

(: append-data-fields ((Listof (String -> Void)) (Listof String) -> Boolean))
(define (append-data-fields appenders fields)
  (if (or (null? appenders)
	  (null? fields))      
      (check-all-data-processed appenders fields)
      (begin 
	((car appenders) (car fields))
	(append-data-fields (cdr appenders) (cdr fields)))))
