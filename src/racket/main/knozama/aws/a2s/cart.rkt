#lang racket

#| Cart accessors for SXML respose from A2S Cart APIs |#

(provide sx-cart-create  ;; get the Cart node for the cart CRUD calls
	 sx-cart-add     ;; create, add, clear, get
	 sx-cart-get
	 sx-cart-clear
	 sx-cart-request-valid
	 sx-cart-id
	 sx-cart-hmac
	 sx-cart-hmac-encoded
	 sx-cart-purchase-url
	 sx-cart-subtotal
	 sx-cart-items
	 sx-cart-item-asin
	 sx-cart-item-quantity
	 sx-cart-item-title
	 sx-cart-item-price
	 sx-cart-item-amount
	 sx-cart-item-total
	 sx-cart-errors
	 sx-cart-error-code
	 sx-cart-error-message)

(require 
 (only-in (planet lizorkin/sxml:2:1/sxml)
	  sxpath)
 (only-in knozama/xml/sxml/utils
	  select-single-node-text)
 (only-in knozama/aws/configuration
	  a2s-ns))

(define ns (list a2s-ns))

;; Cart Access Procedures for A2S Cart SXML 
;; returned by above Cart operations. 
(define sx-cart-create
  (sxpath "/a2s:CartCreateResponse/a2s:Cart" ns))

(define sx-cart-add
  (sxpath "/a2s:CartAddResponse/a2s:Cart" ns))

(define sx-cart-get
  (sxpath "/a2s:CartGetResponse/a2s:Cart" ns))

(define sx-cart-clear
  (sxpath "a2s:CartClearResponse/a2s:Cart" ns))

(define sx-cart-request-valid
  (select-single-node-text "/a2s:Request/a2s:IsValid" ns))

(define sx-cart-id
  (select-single-node-text "/a2s:CartId" ns))

(define sx-cart-hmac 
  (select-single-node-text "/a2s:HMAC" ns))

(define sx-cart-hmac-encoded
  (select-single-node-text "/a2s:URLEncodedHMAC" ns))

(define sx-cart-purchase-url
  (select-single-node-text "/a2s:PurchaseURL" ns))

(define sx-cart-subtotal
  (select-single-node-text "/a2s:SubTotal/a2s:FormattedPrice" ns))

(define sx-cart-items
  (sxpath "/a2s:CartItems/a2s:CartItem" ns))

;; Cart Item
(define sx-cart-item-asin
  (select-single-node-text "/a2s:ASIN" ns))

(define sx-cart-item-quantity
  (select-single-node-text "/a2s:Quantity" ns))

(define sx-cart-item-title
  (select-single-node-text "/a2s:Title" ns))

(define sx-cart-item-price
  (select-single-node-text "/a2s:Price/a2s:FormattedPrice" ns))

(define sx-cart-item-amount 
  (select-single-node-text "/a2s:Amount/a2s:FormattedPrice" ns))

(define sx-cart-item-total
  (select-single-node-text "a2s:ItemTotal/a2s:FormattedPrice" ns))

;; Errors
(define sx-cart-errors 
  (sxpath "/a2s:Request/a2s:Errors/a2s:Error" ns))

(define sx-cart-error-code
  (select-single-node-text "/a2s:Code" ns))

(define sx-cart-error-message
  (select-single-node-text "a2s:Message" ns))
