#lang racket

(provide book-resource)

(require
 (only-in (planet lizorkin/sxml:2:1/sxml)
	  sxpath sxml:text srl:sxml->html)
 (only-in knozama/type/list
	  assoc-value)
 (only-in knozama/web/server/dispatch
	  rest-resource)
 (only-in knozama/web/http/cookies
	  make-cookie)
 (only-in knozama/web/http/http
	  http-send-response)
 (only-in knozama/aws/a2s/a2s
	  similarity-lookup
	  item-lookup)
 (only-in knozama/text/util
	  weave-string-separator)
 (only-in knozama/web/html/html
	  scripts links)
 (only-in knozama/aws/configuration
	  a2s-nss)
 (only-in knozama/site/configure
	  aws-creds)
 (only-in knozama/site/templates
	  amazon-left-vertical
	  ga knozama-header knozama-footer)
 (only-in knozama/site/books/offers
	  item-offers
	  offer-price
	  amazon-new-offer
	  extract-item-offers)
 (only-in knozama/site/books/itemattrs
	  extract-item-attrs)
 (only-in knozama/site/books/itemimages
	  extract-item-imageset)
 (only-in knozama/site/books/reviews
	  extract-editorial-reviews
	  extract-customer-reviews)
 (only-in knozama/site/books/itemattrs
	  item-attributes
	  item-attrs-manufacturer
	  item-attrs-group
	  item-attrs-title
	  item-attrs-authors)
 (only-in knozama/site/books/similarity
	  similar-products-asin
	  similar-products))

;; FIXME RPR - duped in books.sls 
(define item-url
  (select-single-node-text "/a2s:DetailPageURL" a2s-nss))
(define sales-rank
  (select-single-node-text "/a2s:SalesRank" a2s-nss))
(define item-image
  (lambda (item-sxml)
    (assoc-value 'tiny (extract-item-imageset item-sxml))))
(define item-asin
  (let ((sxp (sxpath "/a2s:ASIN" a2s-nss)))
    (lambda (nodelst)
      (sxml:text (sxp nodelst)))))
(define-syntax select-single-node-text
  (syntax-rules ()
    ((_ path-exp ns)
     (let ((sxp (sxpath path-exp ns)))
       (lambda (nodelst)
	 (sxml:text (sxp nodelst)))))))

(define sx-item
  (sxpath "/a2s:ItemLookupResponse/a2s:Items/a2s:Item" a2s-nss))

(define sx-errors
  (sxpath "/a2s:ItemLookupResponse/a2s:Items/a2s:Request/a2s:Errors/a2s:Error" a2s-nss))

(define sx-error-code
  (select-single-node-text "/a2s:Code" a2s-nss))

(define sx-error-msg
  (select-single-node-text "/a2s:Message" a2s-nss))

(define extract-asin-from-remainder
  (lambda (remainder)
    (let ((path-remainder (car remainder)))
      (if (null? path-remainder)
	 #f
	 (car path-remainder)))))

(define book-resource
  (rest-resource
   (GET (lambda (request remainder input-port output-port)
	  (let* ((lookup-sxml (item-lookup aws-creds (extract-asin-from-remainder remainder)))
	       (errors (sx-errors lookup-sxml)))
	    (if (pair? errors) ;; '() if no errors
	       (let ((content (book-error-html errors)))
		 (pretty-print errors)
		 (http-send-response "200-OK" '() output-port 
				     (open-input-string content) 0))
	       (let* ((item-sxml (car (sx-item lookup-sxml)))
		    (item-attrs         (extract-item-attrs    item-sxml))
		    (images             (extract-item-imageset item-sxml))
		    (offers-summary     (extract-item-offers   item-sxml))
		    (amzn-new-offer     (amazon-new-offer (item-offers item-sxml)))
		    (reviews            (extract-customer-reviews item-sxml))
		    (editorial-reviews  (extract-editorial-reviews item-sxml))
		    (similar (sx-item (similarity-lookup aws-creds 
							 (similar-products-asin item-sxml))))
		    (content (book-html images item-attrs reviews editorial-reviews 
					amzn-new-offer similar)))
		 (http-send-response "200-OK" '() output-port
				     (open-input-string content) 0))))))))

(define authors-string
  (lambda (author-lst)
    (weave-string-separator ", " author-lst)))

(define lookup
  (lambda (sym alst)
    (let ((val (assoc sym alst)))
      (if val 
	 (cdr val)
	 ""))))

(define review-html
  (lambda (review)
    ;;(pretty-print "Review HTML")
    ;;(pretty-print review)(newline)
    (let ((location (lookup 'location review)))
      `(div (@ (class "review"))
	    (p (@ (id "summary"))
	       (b "Summary ") ,(lookup 'summary review))
	    (p (b "Name ") ,(lookup 'name review)
	       (b ", Date ") ,(lookup 'date review)
	       ,@(if (zero? (string-length location))
		    '("")
		    `((b ", Location ") ,location)))
	    (p (@ (class "reviewcontent")) ,@(lookup 'content review))))))

(define editorial-review-html
  (lambda (review)
    (let ((source (lookup 'source review)))
      (when (string=? source "Product Description")
	(set! source "Description")) 
      `(div (@ (class "edreview"))
	    (h3 "Editorial Review - " ,source)
	    (p ,@(lookup 'content review))))))

(define similar-desc-html
  (lambda (review)
    `(p (@ (class "similar-desc"))  ,@(lookup 'content review))))

;; search results into an alist
;; alist can be used by json-write
(define similar-item-row
  (lambda (item)
    (let ((asin-value (item-asin item)))
      (let ((url  (item-url  item))  ;; this is amazon url USE IT RPR!
	  (rank (string->number (sales-rank item)))
	  (item-image (item-image item)))
	(let ((asin (string-append "<a href=" url ">" asin-value "</a>")))
	  (let ((attr (item-attributes item)))
	    (let ((author (item-attrs-authors attr))
		(manuf  (item-attrs-manufacturer attr))
		(group  (item-attrs-group attr))
		(title-val (item-attrs-title attr))
		(ed-review (extract-editorial-reviews item)))
	      ;;(pretty-print "EDR")
	      ;;(pretty-print ed-review)
	      (let ((knozama-url (string-append "/books/book/" asin-value)))
		`(div (@ (class "booklistitem"))
		      (image (@ (class "booklistimage")
				(width "72px")
				(height "110px")
				(src ,item-image)) "")
		      (span (@ (class "booklistitemcontent"))
			    (span (@ (class "booklistitemtitle"))
				  (a (@ (class "booklistitemtitle")
					(href ,knozama-url)) ,title-val))
			    (br) " by "
			    (span (@ (class "booklistitemauthor")),(if (pair? author)
								      (car author)
								      "---"))
			    (br)
			    " Sales Rank: "
			    ,rank)
		      ,@(map similar-desc-html ed-review))))))))))

(define book-html
  (lambda (images attrs reviews editorial-reviews amzn-new-offer similar)
    (srl:sxml->html
     `(*TOP*
       (html
	
	(head
	 (title "Knozama - " ,(lookup 'title attrs))
	 ,@(links "http://yui.yahooapis.com/2.5.2/build/fonts/fonts-min.css"
		  "http://yui.yahooapis.com/2.5.2/build/reset-fonts-grids/reset-fonts-grids.css"
		  "http://yui.yahooapis.com/2.5.2/build/base/base-min.css"
		  "http://yui.yahooapis.com/2.5.2/build/assets/skins/sam/skin.css"
		  "http://yui.yahooapis.com/2.5.2/build/tabview/assets/skins/sam/tabview.css"
		  "../../static/knozama/books.css")
	 
	 ,@(scripts "http://yui.yahooapis.com/2.5.2/build/yahoo-dom-event/yahoo-dom-event.js"
		    "http://yui.yahooapis.com/2.5.2/build/element/element-beta-min.js"
		    "http://yui.yahooapis.com/2.5.2/build/container/container_core.js"
		    "http://yui.yahooapis.com/2.5.2/build/menu/menu.js"		  
		    "http://yui.yahooapis.com/2.5.2/build/button/button-min.js"
		    "http://yui.yahooapis.com/2.5.2/build/tabview/tabview-min.js"
		    "../../static/knozama/mainmenunav.js"))
	
	(body (@ (class "yui-skin-sam"))

	      (div (@ (id "doc3") (class "yui-t1"))
		   
		   ,knozama-header
		   
		   (div (@ (id "bd"))			 
			(div (@ (id "yui-main"))
			     (div (@ (class "yui-b"))
				  (div (@ (class "yui-gc"))
				       
				       (div (@ (class "yui-u-first") (id "bookmain"))
					    
					    
					    (div (@ (id "booktabs") (class "yui-navset"))
						 (ul (@ (class "yui-nav"))
						     (li (@ (class "selected")) (a (@ (href "#tab1")) (em "Info")))
						     (li (a (@ (href "##tab2")) (em "Similar")))
						     (li (a (@ (href "#tab3"))  (em "Reviews")))
						     (li (a (@ (href "#tab4"))  (em "Author"))))
						 
						 (div (@ (class "yui-content"))
						      
						      (div (@ (id "tab1"))
							   (div (@ (style "padding: 15px"))
								(div (@ (style "align: left"))
								     (img (@ (src ,(lookup 'medium images)) 
									     (style "margin-right: 20px;float: left"))))
								(div
								 (form (@ (method "post") (action "../../cart"))
								       (input (@ (type "hidden") 
										 (name "asin") 
										 (value ,(lookup 'asin attrs))))
								       (span (@ (id "push-button-addto-cart")
										(class "yui-button yui-push-button"))
									     (span (button (@ (type "submit") (class "first-child")
											      (name "addto-cart-button"))
											   "Add To Cart")))))
								;;								       (input (@ (type "submit") (id "cartaddlink") (name "cartadd") 
								;;										 (value "Add To Cart") (onfocus "blur()"))))
								(p (@ (class "booktitle"))
								   ,(lookup 'title attrs))
								(p (@ (class "bookauthor"))
								   (b "by ") ,(authors-string (lookup 'authors attrs)))
								(p (@ (class "itemattrs"))
								   (span (@ (id "amazon-price-label"))
									 "Your Amazon Price ")
								   (span (@ (id "amazon-price"))
									 ,(offer-price amzn-new-offer)))
								(p (@ (class "itemattrs"))
								   (b "List Price ") ,(lookup 'list-price attrs))
								(p (@ (class "itemattrs"))
								   (b "Publication Date ") ,(lookup 'publication-date attrs))
								(p  (@ (class "itemattrs"))
								    (b "Binding ") ,(lookup 'binding attrs))
								;;(p  (@ (class "itemattrs"))
								;;    (b "Format ") ,(lookup 'format attrs))
								(p  (@ (class "itemattrs"))
								    (b "Customer Rating ") ,(lookup 'rating reviews)
								    " from "  ,(lookup 'count reviews) " customer reviews.")
								(div (@ (class "edreviews"))
								     ,@(map editorial-review-html editorial-reviews))))
						      
						      (div (@ (id "tab2"))
							   (div
							    ,@(map similar-item-row similar)))
						      
						      (div (@ (id "tab3"))
							   (p (b "Average Rating ") ,(lookup 'rating reviews)
							      ", Number of reviews " ,(lookup 'count reviews)
							      ", Pages " ,(lookup 'pages reviews))
							   (div (@ (id "custreviews"))
								,@(map review-html (lookup 'reviews reviews))))
						      
						      (div (@ (id "tab4"))(p "No further author information found."))))))))
			

			(div (@ (class "yui-b"))
			     ,amazon-left-vertical))

		   ,knozama-footer)
	      
	      (script (@ (type "text/javascript"))
		      "(function () {"
		      "var tabView = new YAHOO.widget.TabView('booktabs');"
		      "})();")
	      ,@ga))))))

(define error-html
  (lambda (errors)
    (map (lambda (error)
	   `(div (@ (class "bookerror"))
		 (span ,(sx-error-msg error))))
	 errors)))


(define book-error-html
  (lambda (errors)
    (srl:sxml->html
     `(*TOP*
       (html
	(head
	 (title "Knozama")
	 
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
	
	
	(body (@ (class "yui-skin-sam"))
	      
	      (div (@ (id "doc3") (class "yui-t1"))
		   
		   ,knozama-header
		   
		   (div (@ (id "bd"))
			(div (@ (id "yui-main"))
			     (div (@ (class "yui-b"))
				  
				  (div (@ (class "bookerrors"))
				       (h2 (@ (class "bookerrortitle")) "While looking up your book Amazon responed with an error.")
				       ,@(error-html errors))))
			
			(div (@ (class "yui-b"))
			     ,amazon-left-vertical))
		   
		   ,knozama-footer)	      
	      
	      ,@ga))))))
