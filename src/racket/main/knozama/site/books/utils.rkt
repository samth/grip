#lang racket

(provide select-single-node-text)

(require
 (only-in (planet lizorkin/sxml:2:1/sxml)
	  sxpath sxml:text))

(define-syntax select-single-node-text
  (syntax-rules ()
    ((_ path-exp ns)
     (let ((sxp (sxpath path-exp ns)))
       (lambda (nodelst)
	 (sxml:text (sxp nodelst)))))))
