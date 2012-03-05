#lang typed/racket/base

(provide www-log)
 
(: *www-log* Boolean)
(define *www-log*  #t)		 ;; #f or #t
 
 (define-syntax www-log
   (syntax-rules ()
     ((_ format-str args ...)
      (when *www-log*
	(displayln (format (string-append "~s: " format-str) (current-thread) args ...))))))
