#lang racket

(provide
 item-offers
 amazon-new-offer
 offer-price
 extract-item-offers)

(require
 (only-in knozama/aws/configuration
	  a2s-nss)
 (only-in (planet lizorkin/sxml:2:1/sxml)
	  sxml:text sxpath)
 (only-in knozama/xml/sxml/utils
	  select-single-node-text)
 (only-in knozama/type/list
	  lookup))

(define amazon-merchant-id "ATVPDKIKX0DER")

(define extract-item-offers
  (lambda (item-sxml)
    (let ((offer-summary (item-offer-summary item-sxml))
	(offers        (item-offers item-sxml)))
      ;;(pretty-print "ITEM OFFERS")
      ;;(pretty-print offers)
      `((new-price   . ,(lowest-new-price  offer-summary))
	(used-price  . ,(lowest-used-price offer-summary))))))

(define amazon-new-offer
  (lambda (offers)
    (lookup (lambda (offer)
	      (and (string=? (offer-merchant-id offer)
			   amazon-merchant-id)
		 (string=? (offer-condition offer)
			   "New")))
	    offers
	    '())))

(define item-offer-summary
  (sxpath "/a2s:OfferSummary" a2s-nss))

(define item-offers
  (sxpath "a2s:Offers/a2s:Offer" a2s-nss))

;; Assumes a relative rooted Offer element.
(define offer-condition
  (select-single-node-text "a2s:OfferAttributes/a2s:Condition" a2s-nss))

(define offer-merchant-id
  (select-single-node-text "a2s:Merchant/a2s:MerchantId" a2s-nss))

(define offer-price
  (select-single-node-text "a2s:OfferListing/a2s:Price/a2s:FormattedPrice" a2s-nss))

;; Assumes a relative rooted at OfferSummary element.

(define lowest-new-price
  (select-single-node-text "/a2s:LowestNewPrice/a2s:FormattedPrice" a2s-nss))

(define lowest-used-price
  (select-single-node-text "/a2s:LowestUsedPrice/a2s:FormattedPrice" a2s-nss))

(define total-new
  (select-single-node-text "/a2s:TotalNew" a2s-nss))

(define total-used
  (select-single-node-text "/a2s:TotalUsed" a2s-nss))

;; <OfferSummary>
;;   <LowestNewPrice>
;;     <Amount>801</Amount>
;;     <CurrencyCode>USD</CurrencyCode>
;;     <FormattedPrice>$8.01</FormattedPrice>
;;   </LowestNewPrice>
;;   <LowestUsedPrice>
;;     <Amount>799</Amount>
;;     <CurrencyCode>USD</CurrencyCode>
;;     <FormattedPrice>$7.99</FormattedPrice>
;;   </LowestUsedPrice>
;;   <TotalNew>45</TotalNew>
;;   <TotalUsed>20</TotalUsed>
;;   <TotalCollectible>0</TotalCollectible>
;;   <TotalRefurbished>0</TotalRefurbished>
;; </OfferSummary>
