#lang racket

(provide
 similar-products similar-products-asin)

(require 
 (only-in (planet lizorkin/sxml:2:1/sxml)
	  sxml:text sxpath)
 (only-in knozama/aws/configuration
	  a2s-nss))

(define similar-products
  (sxpath "/a2s:SimilarProducts/a2s:SimilarProduct/a2s:ASIN" a2s-nss))

;; returns a list of similar products asins
;; for a given item-sxml
(define similar-products-asin
  (lambda (item-sxml)
    (map sxml:text (similar-products item-sxml))))

