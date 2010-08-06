#lang racket

(provide
 links scripts)

(require)

(define-syntax links
  (syntax-rules ()
    ((_ alink ...)
     '((link (* (rel "stylesheet") (type "text/css") (href alink)) "")...))))

(define-syntax scripts
  (syntax-rules ()
    ((_ ascript ...)
     `((script (* (type "text/javascript") (src ascript)) "")...))))

