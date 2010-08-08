#| Routines for parsing out values from an A2S Cart Operation |#

#lang racket

(provide
 cart-resource)

(require
 racket/fixnum
 (only-in (planet lizorkin/sxml:2:1/sxml)
	  sxpath sxml:text srl:sxml->html)
 (only-in knozama/std/prelude
	  fx1+)
 (only-in knozama/type/list
	  assoc-value)
 (only-in knozama/web/server/dispatch
	  rest-resource)
 (only-in knozama/web/http/http
	  content-length-or-chunked?
	  http-send-response)
 (only-in knozama/web/http/headers
	  get-header-value
	  SET-COOKIE)
 (only-in knozama/web/http/encoding
	  parse-x-www-form-urlencoded)
 (only-in knozama/web/http/cookies
	  make-cookie
	  parse-cookie)
 (only-in knozama/web/http/headers
	  x-www-form-urlencoded?)
 (only-in knozama/web/html/html
	  scripts links)
 (only-in knozama/web/uri/url/param
	  parse-parms)
 (only-in knozama/aws/a2s/a2s
	  cart-get cart-add cart-create cart-clear)
 (only-in knozama/aws/a2s/cart
	  sx-cart-add sx-cart-get sx-cart-create sx-cart-clear
	  sx-cart-items sx-cart-subtotal sx-cart-purchase-url
	  sx-cart-id sx-cart-hmac-encoded
	  sx-cart-item-asin sx-cart-item-quantity sx-cart-item-title
	  sx-cart-item-amount sx-cart-item-price sx-cart-item-total
	  sx-cart-errors sx-cart-error-message sx-cart-error-code)
 (only-in knozama/site/configure
	  aws-creds
	  HOST)
 (only-in knozama/site/templates
	  amazon-left-vertical
	  ga knozama-header knozama-footer))

#| 
If operation is an add
get ASIN from POST
if cart cookie then
get cart id and hmac from cookie
add item to cart
else
create cart
add item to cart
create cart cookie
|#

(define CART-ID "cid")
(define CART-HMAC "chmac")

;; get the cart header value from the given headers
;; cart header value
(define get-cart-header
  (lambda (headers)
    (get-header-value "Cookie" headers)))

;; Create a cart header from the given id.
;; expires in 10 days.
(define make-cart-cookie-headers
  (lambda (cid chmac)
    (let ((expires (* 60 60 24 10)))
      (list (cons SET-COOKIE
		  (make-cookie HOST "/" CART-HMAC chmac expires))
	    (cons SET-COOKIE
		  (make-cookie HOST "/" CART-ID cid expires))))))

(define is-valid?
  (lambda (cart)
    (string=? (sx-cart-clear cart) "True")))

;; items listof (cons asin qty)
(define add-to-new-cart
  (lambda (item)
    (cart-create aws-creds `((,item . 1)))))

(define parse-request
  (lambda (request)
    (let* ((headers   (cdr request))
	 (cookies   (parse-cookie (get-header-value "Cookie" headers)))
	 (cart-id   (assoc-value CART-ID cookies))
	 (cart-hmac (assoc-value CART-HMAC cookies)))
      (values headers cookies cart-id cart-hmac))))

(define cart-resource
  (rest-resource

   (GET (lambda (request remainder input-port output-port)
	  (let-values (((headers cookies cart-id cart-hmac) (parse-request request)))
	    (let ((cart-response (sx-cart-get (if cart-id
					       (cart-get aws-creds cart-id cart-hmac)
					       '()))))
	      (http-send-response "200-OK" '() output-port
				  (open-input-string (cart-html cart-response #f)) 0)))))
   
   (DELETE (lambda (request remainder input-port output-port)
	     (let-values (((headers cookies cart-id cart-hmac) (parse-request request)))
	       (let ((cart-response (cart-clear aws-creds cart-id cart-hmac)))
		 (http-send-response "204-NO-CONTENT" '() output-port #f 0)))))
   
   (POST (lambda (request remainder input-port output-port)
	   (let-values (((headers cookies cart-id cart-hmac) (parse-request request)))
	     (let* ((form-data (parse-x-www-form-urlencoded input-port))
		  (asin (let ((kv (assoc "asin" form-data)))
			  (if kv (cdr kv) #f))))
	       
	       (if (x-www-form-urlencoded? headers)
		  (let ((cart-response (if cart-id
					(cart-add aws-creds cart-id cart-hmac `((,asin . 1)))
					(cart-create aws-creds `((,asin . 1))))))
		    (let ((cart (if cart-id
				 (sx-cart-add cart-response)
				 (sx-cart-create cart-response))))
		      (pretty-print cart-response)
		      (let ((cart-errors(sx-cart-errors cart)))
			(let ((code (if (not (null? cart-errors))
				     (sx-cart-error-code (car cart-errors)) ;; just get first error code
				     #f)))
			  (pretty-print code)
			  (let ((cart (if (and code (string=? code "AWS.ECommerceService.ItemNotEligibleForCart"))
				       (sx-cart-get (if cart-id
						       (cart-get aws-creds cart-id cart-hmac)
						       '()))
				       cart)))
			    (let ((resp-headers (if cart-id
						 '()
						 (let ((cid (sx-cart-id cart))
						     (hmac (if cart-id
							      cart-hmac
							      (sx-cart-hmac-encoded cart))))
						   (make-cart-cookie-headers cid hmac)))))
			      (http-send-response "200-OK" resp-headers output-port
						  (open-input-string (cart-html cart code)) 0)))))))
		  
		  (http-send-response "415-Unsupport Media Type" '() output-port
				      (open-input-string "Content type for form must be x-www-form-urlencoded.")))))))))

(define shopping-cart-line-html
  (lambda (idx cart-item)
    (let ((tr-attrs (if (fx= idx 1)
		     '(@ (class "cartrow") (id "cartrowfirst"))
		     '(@ (class "cartrow")))))
      `(tr  ,tr-attrs
	    (td (@ (class "cartcell")) ,(sx-cart-item-asin cart-item))
	    (td (@ (class "cartcell")) ,(sx-cart-item-title cart-item))
	    (td (@ (class "cartcell")) ,(sx-cart-item-quantity cart-item))
	    (td (@ (class "cartcell")) ,(sx-cart-item-price cart-item))
	    (td (@ (class "cartcell")) ,(sx-cart-item-total cart-item)))))) 

(define cart-columns-html
  '(tr (@ (class "cartcolumns"))
       (td (@ (class "cartcolumncell")) "ASIN")
       (td (@ (class "cartcolumncell")) "Title" )
       (td (@ (class "cartcolumncell")) "Qty" )
       (td (@ (class "cartcolumncell")) "Amount")
       (td (@ (class "cartcolumncell")) "Total")))

(define cart-summary-html
  (lambda (cart)
    `(tr (@ (class "cartsumrow"))
	 (td (@ (class "cartsumcell")) " ")
	 (td (@ (class "cartsumcell")) " ")
	 (td (@ (class "cartsumcell")) " ")
	 (td (@ (class "cartsumcell")) " ")
	 (td (@ (class "cartsumcell")) ,(sx-cart-subtotal cart)))))

(define generate-cart-line-html
  (lambda (items)
    (let loop ((idx 1) (items items) (accum (list cart-columns-html)))
      (if (null? items)
	 accum
	 (loop (fx1+ idx) (cdr items) (cons (shopping-cart-line-html idx (car items)) accum))))))

(define shopping-cart-html
  (lambda (cart)
    (let* ((items (sx-cart-items cart)))
      `(table (@ (class "carttable"))
	      ,@(reverse (cons (cart-summary-html cart) (generate-cart-line-html items)))))))

(define checkout-blurb
  '(div (@ (class "checkoutblurb"))
	(p "Knozama is an Amazon affiliate."
	   "  By clicking on the above link your shopping cart is transfered to Amazon for checkout."
	   "  The checkout process takes place "
	   (b "soley")
	   " on Amazon's site."
	   "  Knozama never prompts for or retains your Amazon account information or credit cards.")
	(p "When checking out at Amazon you will have an opportunity to "
	   (b "review") " ," (b "modify") " or " (b "abandon")
	   " your order prior to placement."
	   "  After placement your order may be tracked or canceled through Amazon's site.")))

(define error-html
  (lambda (code)
    (if (and code (string=? code "AWS.ECommerceService.ItemNotEligibleForCart"))
       '(div (@ (class "checkoutblurb"))
	     (p (@ (class "carterrorsheader"))
		"We are sorry.  We received an error message from Amazon. The item is ineligible for adding to a cart."))
       '(div (p "")))))

(define cart-html
  (lambda (cart code)
    (pretty-print cart)
    (srl:sxml->html
     `(*TOP*
       (html
	
	(head
	 (title "Knozama.com")
	 
	 ,@(links "http://yui.yahooapis.com/2.5.2/build/fonts/fonts-min.css"
		  "http://yui.yahooapis.com/2.5.2/build/reset-fonts-grids/reset-fonts-grids.css"
		  "http://yui.yahooapis.com/2.5.2/build/base/base-min.css"
		  "http://yui.yahooapis.com/2.5.2/build/assets/skins/sam/skin.css"
		  "http://yui.yahooapis.com/2.5.2/build/button/assets/skins/sam/button.css"
		  "../../static/knozama/books.css")
	 
	 ,@(scripts "http://yui.yahooapis.com/2.5.2/build/yahoo-dom-event/yahoo-dom-event.js"
		    "http://yui.yahooapis.com/2.5.2/build/element/element-beta-min.js"
		    "http://yui.yahooapis.com/2.5.2/build/container/container_core.js"
		    "http://yui.yahooapis.com/2.5.2/build/button/button-min.js"
		    "http://yui.yahooapis.com/2.5.2/build/event/event-min.js"
		    "http://yui.yahooapis.com/2.5.2/build/connection/connection-min.js"
		    "http://yui.yahooapis.com/2.5.2/build/menu/menu.js"		  
		    "../../static/knozama/mainmenunav.js"))	 
	
	(body 
	 (@ (class "yui-skin-sam"))
	 
	 (div 
	  (@ (id "doc3") (class "yui-t1"))
	  
	  ,knozama-header
	  
	  (div (@ (id "bd"))
	       (div (@ (id "yui-main"))
		    (div (@ (class "yui-b"))

			 (div (@ (class "yui-gc"))
			      
			      (div (@ (class "yui-u-first") (id "cartmain"))
				   (p (@ (class "cartheader")) "Your Knozama Shopping Cart")
				   
				   ,(shopping-cart-html cart)
				   
				   (div 
				    (p (@ (class "cartbuttons"))
				       (span (@ (id "link-button-checkout") 
						(class "yui-button yui-link-button"))
					     (span (@ (class "first-child"))
						   (a (@ (class "cartcheckoutlink")
							 (tabindex 1)
							 (href ,(sx-cart-purchase-url cart)))
						      "Checkout at Amazon")))
				       (span (@ (id "push-button-empty-cart") 
						(class "yui-button yui-push-button"))
					     (span (button (@ (type "button") (class "first-child")
							      (name "empty-cart-button"))
							   "Empty Cart")))))
				   
				   ,(error-html code)
				   
				   ,checkout-blurb))))
	       
	       (div (@ (class "yui-b")) ,amazon-left-vertical))
	  
	  ,knozama-footer)
	 
	 ;; use DOM ready event so I can put this back atop in the headers, deferred as well
	 (script (@ (src "../../static/knozama/cart.js")) "")
	 ,@ga))))))

