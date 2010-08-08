#lang racket

(provide 
 extract-customer-reviews
 extract-editorial-reviews)

(require
 (only-in knozama/aws/configuration
	  a2s-nss)
 (only-in (planet neil/htmlprag:1:6)
	  html->sxml)
 (only-in (planet lizorkin/sxml:2:1/sxml)
	  sxpath sxml:text))

(define-syntax select-single-node-text
  (syntax-rules ()
    ((_ path-exp ns)
     (let ((sxp (sxpath path-exp ns)))
       (lambda (nodelst)
	 (sxml:text (sxp nodelst)))))))

;; Reviews for the give item-asin
(define extract-item-reviews
  ;;   (lambda (item)
  ;;     (let ((asin (item-asin item))
  (lambda (asin)
    (let ((reviews (map extract-item-editorial-review asin))) ;;(sx-editorial-reviews item))))
      (cons asin reviews))))

(define item-reviews
  (lambda (items)
    (map extract-item-reviews items)))

;; extract a item's editorial review
;; returns '(review (source . "...") (content . "..."))
(define extract-item-editorial-review
  (lambda (review)
    (let ((source  (sx-editorial-source review))
	(content (sx-editorial-content review)))
      (list 'review 
	    (cons 'source source)
	    (cons 'content content)))))

(define sx-editorial-reviews
  (sxpath "/a2s:EditorialReviews/a2s:EditorialReview" a2s-nss))

(define sx-editorial-review-source
  (select-single-node-text "/a2s:Source" a2s-nss))

(define sx-editorial-source
  (select-single-node-text "/a2s:Source" a2s-nss))

(define sx-editorial-content
  (select-single-node-text "/a2s:Content" a2s-nss))

(define sx-average-rating
  (select-single-node-text "/a2s:AverageRating" a2s-nss))

(define sx-total-reviews
  (select-single-node-text "/a2s:TotalReviews" a2s-nss))

(define sx-total-review-pages
  (select-single-node-text "/a2s:TotalReviewPages" a2s-nss))

(define sx-customer-reviews
  (sxpath "/a2s:CustomerReviews" a2s-nss))

(define sx-item-reviews
  (sxpath "/a2s:Review" a2s-nss))

(define sx-review-name
  (select-single-node-text "/a2s:Reviewer/a2s:Name" a2s-nss))

(define sx-review-location
  (select-single-node-text "/a2s:Reviewer/a2s:Location" a2s-nss))

(define sx-review-date
  (select-single-node-text "/a2s:Date" a2s-nss))

(define sx-review-summary
  (select-single-node-text "/a2s:Summary" a2s-nss))

(define sx-review-content
  (select-single-node-text "/a2s:Content" a2s-nss))

;; Review strings have embedded html in them.
;; Parse the review strings into SXML for later splicing into place.
;; Otherwise the tags are literally embedded in the displayed review text
;; in the browser.
(define parse-review-content
  (lambda (review-str)
    (cdr (html->sxml (open-input-string review-str)))))

(define extract-review
  (lambda (review)
    (let ((name      (sx-review-name review))
	(location  (sx-review-location review))
	(date      (sx-review-date review))
	(summay    (sx-review-summary review))
	(content   (sx-review-content review)))
      (let ((sxml-content (parse-review-content content)))
	(list (cons 'name name)
	      (cons 'location location)
	      (cons 'date date)
	      (cons 'summary summay)
	      (cons 'content sxml-content))))))

(define extract-editorial-review
  (lambda (review)
    (let ((source  (sx-editorial-review-source review))
	(content (sx-review-content review)))
      (let ((sxml-content (parse-review-content content)))
	(list (cons 'source source)
	      (cons 'content sxml-content))))))

(define extract-editorial-reviews
  (lambda (item-sxml)
    (let ((editorial-reviews-sxml (sx-editorial-reviews item-sxml)))
      (map extract-editorial-review editorial-reviews-sxml))))

(define extract-customer-reviews
  (lambda (item-sxml) 
    ;;     (pretty-print (sx-customer-reviews item-sxml))
    (let ((customer-reviews (sx-customer-reviews item-sxml)))
      (if (null? customer-reviews)
	 (list (cons 'rating "N/A")
	       (cons 'pages  "0")
	       (cons 'count  "0")
	       (cons 'reviews (list (list (cons 'source "knozama")
					  (cons 'content (list "No reviews for this book were found."))))))
	 (let ((customer-reviews (car customer-reviews)))
	   (let ((pages     (sx-total-review-pages customer-reviews))
	       (count     (sx-total-reviews customer-reviews))
	       (rating    (sx-average-rating customer-reviews))
	       (reviews-sxml  (sx-item-reviews customer-reviews)))
	     (let ((reviews (map extract-review reviews-sxml)))
	       (list (cons 'rating rating) 
		     (cons 'pages pages) 
		     (cons 'count count)
		     (cons 'reviews reviews)))))))))
