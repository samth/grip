#lang racket

(provide
 item-attrs-authors
 item-attrs-group
 item-attrs-title
 item-attrs-manufacturer
 item-attributes
 extract-item-attrs)

(require
 (only-in knozama/aws/configuration
	  a2s-nss)
 (only-in knozama/site/books/utils
	  select-single-node-text)
 (only-in (planet lizorkin/sxml:2:1/sxml)
	  sxpath sxml:text))

(define item-asin
  (select-single-node-text "/a2s:ASIN" a2s-nss))

(define item-url
  (select-single-node-text "/a2s:DetailPageURL" a2s-nss))

(define sales-rank
  (select-single-node-text "/a2s:SalesRank" a2s-nss))

(define item-attributes
  (sxpath "/a2s:ItemAttributes" a2s-nss))

(define sx-item-attrs-authors
  (sxpath "/a2s:Author" a2s-nss))

(define item-list-price
  (select-single-node-text "/a2s:ListPrice/a2s:FormattedPrice" a2s-nss))

(define item-attrs-manufacturer
  (select-single-node-text "/a2s:Manufacturer" a2s-nss))

(define item-attrs-group
  (select-single-node-text "/a2s:ProductGroup" a2s-nss))

(define item-attrs-title
  (select-single-node-text "/a2s:Title" a2s-nss))

(define item-attrs-binding
  (select-single-node-text "/a2s:Binding" a2s-nss))

(define item-attrs-format
  (select-single-node-text "a2s:Format" a2s-nss))

(define item-attrs-authors
  (lambda (item-attrs)
    (let ((authors (sx-item-attrs-authors item-attrs)))
      (map sxml:text authors))))

(define item-attrs-publication-date
  (select-single-node-text "a2s:PublicationDate" a2s-nss))

;; returns an alist of item attributes
;; search results into an alist
;; alist can be used by json-write
(define extract-item-attrs
  (lambda (item)
    (let ((attr (item-attributes item)))
      (let ((asin       (cons 'asin        (item-asin item)))
	  (authors    (cons 'authors     (item-attrs-authors attr)))
	  (manuf      (cons 'manuf       (item-attrs-manufacturer attr)))
	  (group      (cons 'group       (item-attrs-group attr)))
	  (title      (cons 'title       (item-attrs-title attr)))
	  (binding    (cons 'binding     (item-attrs-binding attr)))
	  (format     (cons 'format      (item-attrs-format attr)))
	  (list-price (cons 'list-price  (item-list-price attr)))
	  (publication-date (cons 'publication-date (item-attrs-publication-date attr))))
	
	(list asin title authors publication-date manuf binding format group list-price)))))

