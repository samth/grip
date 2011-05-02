#lang typed/racket/base

(provide keyword-search
	 browse-node-search)

(require
 racket/pretty
 (only-in (planet knozama/webkit:1:0/web/uri)
	  make-uri
	  url-encode-string)
 (only-in (planet knozama/webkit:1:0/web/uri/url/param)
	  parms->query)
 (only-in (planet knozama/webkit:1:0/web/http/http11)
	  http-invoke)
 (only-in "../credential.rkt"
	  aws-credential-associate-tag
	  aws-credential-secret-key
	  aws-credential-access-key)
 (only-in (planet knozama/common:1:0/type/date)
	  current-time-iso-8601)
 (only-in "../configuration.rkt"
	  a2s-ns
	  a2s-host)
 "a2s.rkt")

(define search-op-parm
  '("Operation" . "ItemSearch"))

(define index-parm
  (lambda (sym)
    (case sym
      ((KINDLE)
       '("SearchIndex" . "KindleStore"))
      ((BOOKS)
       '("SearchIndex" . "Books"))
      (else '("SearchIndex" . "All")))))

;;       '("SearchIndex" . "Books&Power=binding:Kindle Edition"))

(define group
  (lambda (sym)
    (case sym
      ((Attributes)	"ItemAttributes")
      ((Nodes)          "BrowseNodes")
      ((Offer)          "OfferSummary")
      ((Rank)		"SalesRank")
      ((Small)		"Small")
      ((Large)          "Large")
      ((Review)		"EditorialReview")
      ((Ids)		"ItemIds"))))

;;      ((Images)		"Images")

(define rank
  (lambda (sym)
    (case sym
      ((PriceAsc)	"price")
      ((PriceDesc)	"-price")
      ((Review)		"reviewrank")
      ((Date)		"daterank")
      ((Sales)		"salesrank"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; GIven a list of group symbols form the ResponseGroup kv param and url-encode it.
;; listof (symbol?) -> string?
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define response-group-parm
  (lambda (groups)
    (let loop ((groups groups)(param ""))
      (if (null? groups)
	 `("ResponseGroup" . ,(url-encode-string param #f))
	 (loop (cdr groups)
	       (let ((g (group (car groups))))
		 (if (void? g)
		    param
		    (if (equal? param "")
		       g
		       (string-append param "," g)))))))))

(define browse-node-search

  (lambda (index groups node power by page)

  (define add-sort
    (lambda (param)
      (if by
	 (cons `("Sort" . ,(rank by)) param)
	 param)))

  (define add-power
    (lambda (param)
      (if power
	 (cons `("Power" . ,power) param)
	 param)))

    (let* ((base
	 (cons `("BrowseNode" . ,(number->string node))
	       (cons search-op-parm 
		     (cons `("ItemPage" . ,(number->string page))
			   (cons (response-group-parm groups) 
				 (list (index-parm index)))))))
	(parms (add-power (add-sort base))))

      (displayln parms)
      (a2s-invoke parms))))

(define keyword-search
  (lambda (index groups words) 
    (let ((parms 
	 (cons `("Keywords" . ,(url-encode-string words #f))
	       (cons search-op-parm
		     (cons (response-group-parm groups) (list (index-parm index)))))))
      (a2s-invoke parms))))
