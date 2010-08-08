#lang racket

(provide reviews-resource)

(require
 (only-in (planet neil/htmlprag:1:6)
	  html->sxml)
 (only-in (planet lizorkin/sxml:2:1/sxml)
	  sxpath sxml:text srl:sxml->html)
 (only-in knozama/web/uri
	  uri->string ;; debug
	  uri-authority
	  authority-host
	  make-uri)
 (only-in knozama/web/uri/url/param
	  parms->query)
 (only-in knozama/web/server/dispatch
	  rest-resource)
 (only-in knozama/web/http/http
	  content-length-or-chunked?
	  http-send-response)
 (only-in knozama/web/html/html
	  scripts links)
 (only-in knozama/site/templates
	  amazon-left-vertical
	  ga knozama-header knozama-footer)
 (only-in knozama/api/feedburner
	  feedburner-rss)
 (only-in knozama/web/rss/rss20/rss
	  fetch-rss sx-link sx-items sx-title sx-description
	  sx-media-content sx-media-content-url sx-media-content-medium
	  sx-media-content-width sx-media-content-height))

(define no-parms (parms->query '()))



;;"http://rss.mnginteractive.com/live/DenverPost/DPO_37.xml"
(define denverpost-uri
  (make-uri "http" #f "rss.mnginteractive.com" #f "/live/DenverPost/DPO_37.xml"
	    no-parms ""))

;; Boston.com is RSS 1.0 NEED TO ADD PARSING
;; http://www.boston.com/ae/books?mode=rss_10
(define boston.com-uri
  (make-uri "http" #f "www.boston.com" #f "/ae/books"
	    (parms->query (list '("mode" . "rss_10"))) ""))

;; feedburner
(define sfgate-uri
  "sfgate/rss/feeds/books")

;;feedburner
(define reviews-rss
  '("/nyrb"))

;; feedburner
(define bookswriters
  "/newwest/topic/bookswriters")

(define miami-herald-uri
  (make-uri "http" #f "www.miamiherald.com" #f "/215/index.xml" no-parms ""))

(define npr-reviews-uri
  (make-uri "http" #f "www.npr.org" #f "/rss/rss.php" (parms->query (list '("id" . "1032"))) ""))

(define nytimes-uri
  (make-uri "http" #f "www.nytimes.com" #f "/services/xml/rss/nyt/Books.xml" no-parms ""))

(define reviews-resource
  (rest-resource
   (GET (lambda (request remainder input-port output-port)
	  (let ((nyrb-rss           (sx-items (feedburner-rss (car reviews-rss))))
	      (bookswriters-rss   (sx-items (feedburner-rss bookswriters)))
	      (sfgate-rss         (sx-items (feedburner-rss sfgate-uri)))
	      (npr-rss            (sx-items (fetch-rss npr-reviews-uri)))
	      (miami-rss          (sx-items (fetch-rss miami-herald-uri)))
	      (nytimes-rss        (sx-items (fetch-rss nytimes-uri)))
	      (denverpost-rss     (sx-items (fetch-rss denverpost-uri))))
	    ;;	       (boston.com-rss (pretty-print (fetch-rss boston.com-uri))))
	    (http-send-response "200-OK" '() output-port
				(open-input-string (reviews-html miami-rss npr-rss nyrb-rss 
								 bookswriters-rss nytimes-rss
								 sfgate-rss denverpost-rss
								 '()))
				0))))))

;; given a list of media generate html for the FIRST media element only (for now)
(define media-content-html
  (lambda (sxml-medias) ;; list of media     
    (if (null? sxml-medias)
       " "
       (let ((sxml-media (car sxml-medias)))
	 (let ((medium (sx-media-content-medium sxml-media)))
	   (if (and (string? medium)
		 (string=? medium "image"))
	      (let ((mk-dimen (lambda (sz)
			      (string-append sz " px"))))
		`(image (@ (class  "reviewsitemimage")
			   (src    ,(sx-media-content-url sxml-media))
			   (width  ,(mk-dimen (sx-media-content-width sxml-media)))
			   (height ,(mk-dimen (sx-media-content-height sxml-media))))))
	      " "))))))

(define item-html
  (lambda (item)
    (let ((sxml-description (html->sxml (open-input-string (sx-description item))))
	(sxml-media-content (sx-media-content item)))
      (media-content-html sxml-media-content)
      `(div (@ (class "reviewsitem"))
	    ,(media-content-html sxml-media-content)
	    (p (@ (class "reviewsitemtitle")) 
	       (a (@ (href ,(sx-link item)) (target "_blank"))
		  ,(sx-title item)))
	    (p (@ (class "reviewsitemdes"))
	       ,@(cdr sxml-description))))))

(define rss-items-html
  (lambda (items)
    (if (null? items)
       '((div (@ (class "reviewsitemtitle"))
	      (p "Reviews are currently unavailable.")))
       (map item-html items))))

(define reviews-html
  (lambda (miami-rss npr-rss nyrb-rss bookswriters-rss nytimes-rss sfgate-rss denverpost-rss boston.com-rss)
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
		    "http://yui.yahooapis.com/2.5.2/build/menu/menu.js"
		    "http://yui.yahooapis.com/2.5.2/build/tabview/tabview-min.js"
		    "../../static/knozama/mainmenunav.js"))
	
	(body (@ (class "yui-skin-sam"))
	      
	      (div (@ (id "doc3") (class "yui-t1"))
		   
		   ,knozama-header
		   
		   (div (@ (id "bd"))
			(div (@ (id "yui-main"))
			     (div (@ (class "yui-b"))
				  
				  (div (@ (class "yui-u-first") (id "bookmain"))
				       
				       
				       (div (@ (id "reviewtabs") (class "yui-navset"))

					    (ul (@ (class "yui-nav"))
						(li (@ (class "selected")) (a (@ (href "#tab1")) (em "NY Times")))
						(li (a (@ (href "#tab2"))  (em "NPR:Books")))
						(li (a (@ (href "#tab3"))  (em "Boston.com")))
						(li (a (@ (href "#tab4"))  (em "Denver Post")))
						(li (a (@ (href "#tab5"))  (em "Miami Herald")))
						(li (a (@ (href "#tab6"))  (em "SFGate")))
						(li (a (@ (href "#tab7"))  (em "New West")))
						(li (a (@ (href "#tab8"))  (em "NY Review"))))
					    
					    (div (@ (class "yui-content"))						  
						 (div (@ (id "tab1"))
						      ,@(rss-items-html nytimes-rss))						  
						 (div (@ (id "tab2"))
						      ,@(rss-items-html npr-rss))				  
						 (div (@ (id "tab3"))
						      ,@(rss-items-html boston.com-rss))						  
						 (div (@ (id "tab4"))						       
						      ,@(rss-items-html denverpost-rss))
						 (div (@ (id "tab5"))
						      ,@(rss-items-html miami-rss))
						 (div (@ (id "tab6"))
						      ,@(rss-items-html sfgate-rss))						  
						 (div (@ (id "tab7"))
						      ,@(rss-items-html bookswriters-rss))						  
						 (div (@ (id "tab8"))
						      ,@(rss-items-html nyrb-rss)))))))
			
			(div (@ (class "yui-b"))
			     ,amazon-left-vertical))
		   
		   ,knozama-footer)

	      (script (@ (type "text/javascript"))
		      "(function () {"
		      "var tabView = new YAHOO.widget.TabView('reviewtabs');"
		      "})();")
	      
	      ,@ga))))))

