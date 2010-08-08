#| Book Toues |#

#lang racket

(provide
 tours-resource)

(require
 (only-in (planet lizorkin/sxml:2:1/sxml)
	  sxpath sxml:text srl:sxml->html
	  srl:select-kids)
 (only-in knozama/std/control
	  aif)
 (only-in knozama/type/list
	  assoc-value)
 (only-in knozama/web/http/headers
	  SET-COOKIE
	  get-cookie-header)
 (only-in  knozama/web/http/encoding
	   parse-x-www-form-urlencoded)
 (only-in knozama/web/http/http
	  content-length-or-chunked?
	  http-send-response)
 (only-in knozama/web/http/cookies
	  make-cookie
	  parse-cookie)
 (only-in knozama/web/server/dispatch
	  rest-resource)
 (only-in knozama/web/html/html
	  scripts links)
 (only-in knozama/web/rss/rss20/rss
	  sx-link
	  sx-title sx-description
	  sx-items)
 (only-in knozama/xml/sxml/utils
	  select-single-node-text)
 (only-in knozama/web/html/htmlprag
	  html->sxml)
 (only-in knozama/xml/ssax/ssax
	  xml->sxml)
 (only-in knozama/api/booktour
	  booktour-search)
 (only-in knozama/site/configure
	  HOST)
 (only-in knozama/site/templates
	  amazon-left-vertical
	  ga knozama-header knozama-footer))

(define ZIP "zip") ;; cookie key

(define tour-distance "50")
(define tour-time "30")

(define make-zip-cookie-header
  (lambda (zip)
    (let ((expires (* 60 60 24 10)))
      (cons SET-COOKIE
	    (make-cookie HOST "/" ZIP zip expires)))))

(define (parse-request request)
  (let* ((headers  (cdr request))	
       (cookies  (parse-cookie (get-cookie-header headers)))
       (zipcode  (assoc-value ZIP cookies)))
    (values headers cookies zipcode)))

(define tours-resource
  (rest-resource

   (GET (lambda (request remainder input-port output-port)
	  (let-values (((headers cookies zipcode) (parse-request request)))
	    (let ((tour-content (if zipcode
				 (booktour-search zipcode tour-distance 
						  "Home" tour-time)
				 '()))
		(resp-headers '()))
	      (http-send-response "200-OK" resp-headers output-port
				  (open-input-string(tours-main-html zipcode tour-content)) 0)))))

   (POST (lambda (request remainder input-port output-port)
	   (let-values (((headers cookies zipcode) (parse-request request)))
	     (let ((form-data (parse-x-www-form-urlencoded input-port)))
	       (let ((zipcode (assoc-value "zip" form-data)))
		 (let ((resp-headers (if zipcode					       
				      (list (make-zip-cookie-header zipcode))
				      '()))
		     (tour-content (if zipcode
				      (booktour-search zipcode tour-distance
						       "Home" tour-time)
				      '())))
		   (http-send-response "200-OK" resp-headers output-port
				       (open-input-string (tours-main-html zipcode tour-content)) 0)))))))))

(define text (srl:select-kids string?))

(define sx-book-title
  (select-single-node-text "i" '()))

;; returns listof (title location date address)
(define (parse-description desc-str)
  (let ((info (html->sxml (open-input-string desc-str))))
    (let ((text-nodes (cdr (text info)))
	(title (sx-book-title info)))
      (let ((loc (car text-nodes)))
	(cons title 
	      (cons (substring loc 12 (string-length loc))
		    (cdr text-nodes)))))))

(define tour-html
  (lambda (rss-item)
    (let ((info (parse-description (sx-description rss-item)))
	(author-link (sx-link rss-item)))
      `(p (* (class "booktour"))
	  (b (a (* (href ,author-link)) ,(sx-title rss-item)))  ;; rrs title is the author
	  " author of " (i ,(car info)) (br)
	  " -- is at " ,(cadr info) ", " ,(cadddr info)
	  " on " ,(caddr info)))))

(define (tours-html rss)
  (map tour-html (sx-items rss)))

(define (tours-main-html zipcode tours-sxml)
  (srl:sxml->html
   `(*TOP*
     (html
      
      (head
       (title "Knozama")
       
       ,@(links "http://yui.yahooapis.com/2.5.2/build/fonts/fonts-min.css"
		"http://yui.yahooapis.com/2.5.2/build/reset-fonts-grids/reset-fonts-grids.css"
		"http://yui.yahooapis.com/2.5.2/build/base/base-min.css"
		"http://yui.yahooapis.com/2.5.2/build/assets/skins/sam/skin.css"
		"http://yui.yahooapis.com/2.5.2/build/calendar/assets/skins/sam/calendar.css"
		"http://yui.yahooapis.com/2.5.2/build/menu/assets/skins/sam/menu.css"
		"../static/knozama/books.css")
       
       ,@(scripts "http://yui.yahooapis.com/2.5.2/build/yahoo-dom-event/yahoo-dom-event.js"
		  "http://yui.yahooapis.com/2.5.2/build/container/container_core.js"
		  "http://yui.yahooapis.com/2.5.2/build/menu/menu.js"
		  "http://yui.yahooapis.com/2.5.2/build/calendar/calendar-min.js"
		  "../static/knozama/mainmenunav.js"))
      
      (body (@ (class "yui-skin-sam"))
	    
	    (div (@ (id "doc3") (class "yui-t1"))
		 
		 ,knozama-header
		 
		 (div (@ (id "bd"))
		      
		      (div (@ (id "yui-main"))
			   (div (@ (class "yui-b"))
				
				(div (@ (class "tours-blk"))
				     (div (@ (id "zipform"))
					  (h3 "Local Area Book Tours from "
					      (a (@ (href "http://booktour.com") (target "_blank"))
						 (b "booktour.com")))
					  
					  
					  (div (@ (id "cal1Container")) (br))
					  
					  (form (@ (action "/tours")
						   (method "POST")
						   (id "tourform"))
						(label (@ (for "zipid")) "Zipcode")
						(input (@ (id "zipid")
							  (type "text")
							  (name "zip")
							  (value ,(aif zipcode it ""))))
						(input (@ (type "submit")))))				
				     
				     (script (@ (type "text/javascript") (src "../../static/knozama/cal.js")) "")
				     
				     ,@(tours-html tours-sxml))))
		      
		      (div (@ (class "yui-b"))
			   ,amazon-left-vertical))
		 
		 ,knozama-footer)
	    ,@ga)))))

