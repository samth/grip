#lang racket

(provide doc)

(require 
 (planet lizorkin/sxml:2:1/sxml)
 knozama/web/uri
 knozama/bts/slcrawl)

(define url-test (uri "http" (authority #f "connect.pulaski.k12.wi.us" #f)  "/fairview/kam_supplies.cfm" #f #f))

(define select-anchor 
  (lambda (tree) 
    (select (lambda (x)
	      (eq? x 'a))
	    tree)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Transform function to select all the anchor tags
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (define xform
;;   `((*default* . ,(lambda (x . rest) 
;;                     (select (lambda (x) 
;; 			      (eq? x 'a)) 
;; 			    rest)))
;;     (*TOP* . ,(lambda (x . rest)
;; 		(cons x (select (lambda (x) 
;; 				  (eq? x 'a)) rest))))
;;     (*DECL* . ,(lambda (x . rest)
;; 		 '()))
;;     (& . ,(lambda x '()))
;;     (a 
;;      ((@ ((href . ,(lambda x x))
;;           (*default* . ,(lambda x '()))) . ,(lambda (tag . kids) kids))
;;       (*default* . ,(lambda (x . rest) 
;;                       (select (lambda (x) (or (eq? x 'href) (eq? x '*text*))) rest)))
;;       (*text* . ,(lambda x x))
;;       (a *macro* . ,(lambda x x))) . ,(lambda (tag . kids) 
;; 				   (cons tag (select (lambda (x) (or (eq? x 'href) (eq? x '*text*)))
;; 						     kids))))))

(define xform-anchors
  `((*TOP* . ,(lambda (x . rest)
		(select-anchor rest)))
    (a ((@ ((href . ,(lambda x x))
	    (*default* . ,(lambda x x))) . ,(lambda (x . rest) rest))
	(*text* . ,(lambda x x))
	(*default* . ,(lambda x '()))) . ,(lambda (anchor . kids)
				       (cons anchor (select (lambda (x)
							      (or (eq? x 'href)
								 (eq? x '*text*)))
							    kids))))
    (*text* . ,(lambda x '()))
    (*default* . ,(lambda (x . rest)
		    (select-anchor rest)))))

(define (doc)
  (transform-sxmldoc (fetch-url url-test) xform-anchors))

