#lang racket/base

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
	 sx-cart-error-message
	 cart-create 
	 cart-clear cart-get 
	 cart-add)

(require 
 (only-in (planet knozama/common:1/text/util)
	  weave-string-separator)
 (only-in (planet lizorkin/sxml:2:1/sxml)
	  sxpath)
 (only-in (planet knozama/xml:1/util)
	  select-single-node-text)
 (only-in "a2s.rkt"
	  a2s-invoke)
 (only-in "../configuration.rkt"
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

#|  Cart API |#

;; START HERE -- TEST CART CREATE

;;  (define cart-modify #f)

;; Given a line number and an alist of (asin . qty) create a cart line
;; Item.1.ASIN=123&Item.1.Quantity=10
(define make-cart-lines
  (lambda (entries)
    (let ((make-line (lambda (n line)
		       (let ((line-no (number->string n)))
			 (string-append "Item."   line-no ".ASIN=" (car line)
					"&Item."  line-no ".Quantity=" (number->string (cdr line)))))))
      (do ((accum '() (cons (make-line n (car entries)) accum))
	   (entries entries (cdr entries))
	   (n 1 (add1 n)))
	  ((null? entries) (weave-string-separator "&" (reverse accum)))))))


;; lines : alist of (item . qty)
(define cart-create 
  (lambda (creds items)
    (let ((parms `(("Operation" . "CartCreate"))))
      (a2s-invoke creds parms (make-cart-lines items)))))

(define cart-parms
  (lambda (operation cart-id hmac-encoded)
    `(("Operation" . ,operation)
      ("CartId"    . ,cart-id)
      ("HMAC"      . ,hmac-encoded))))

(define cart-clear
  (lambda (creds cart-id hmac-encoded)
    (a2s-invoke creds (cart-parms "CartClear" cart-id hmac-encoded) #f)))

(define cart-get
  (lambda (creds cart-id hmac-encoded)
    (a2s-invoke creds (cart-parms "CartGet" cart-id hmac-encoded) #f)))

(define cart-add
  (lambda (creds cart-id hmac-encoded items)
    (a2s-invoke creds 
		(cart-parms "CartAdd" cart-id hmac-encoded)
		(make-cart-lines items))))

