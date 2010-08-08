#lang racket

(provide awards-resource)

(require 
 (only-in (planet lizorkin/sxml:2:1/sxml)
	  sxpath sxml:text srl:sxml->html)
 (only-in knozama/web/server/dispatch
	  rest-resource)
 (only-in knozama/web/http/http
	  content-length-or-chunked?
	  http-send-response)
 (only-in knozama/web/html/html
	  scripts links)
 (only-in knozama/site/templates
	  amazon-left-vertical
	  ga knozama-header knozama-footer))

(define awards-resource
  (rest-resource
   (GET (lambda (request remainder input-port output-port)
	  (http-send-response "200-OK" '() output-port
			      (open-input-string (awards-html))
			      0)))))
(define awards-html
  (lambda ()
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
				  
				  (p "Blank Page")))
			
			(div (@ (class "yui-b"))
			     ,amazon-left-vertical))
		   
		   ,knozama-footer)
	      
	      
	      ,@ga))))))