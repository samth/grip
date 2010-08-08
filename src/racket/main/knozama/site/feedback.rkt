#lang racket

(provide feedback-resource)

(require 
 (only-in (planet lizorkin/sxml:2:1/sxml)
	  sxpath sxml:text srl:sxml->html)
 (only-in knozama/type/list
	  assoc-value)
 (only-in knozama/web/server/dispatch
	  rest-resource)
 (only-in knozama/web/http/http
	  content-length-or-chunked?
	  http-send-response)
 (only-in knozama/web/http/encoding
	  parse-x-www-form-urlencoded)
 (only-in knozama/web/html/html
	  scripts links)
 (only-in knozama/site/templates
	  amazon-left-vertical
	  ga knozama-header knozama-footer)
 (only-in knozama/site/configure
	  feedback-path))

(define (store-feedback formdata)
  (let ((filename (string-append feedback-path "/feedback-" 
			       (number->string (current-milliseconds))
			       ".txt")))
    (call-with-output-file filename
      (lambda (op)
	(write formdata op)))))

(define feedback-resource
  (rest-resource
   (GET (lambda (request remainder input-port output-port)
	  (http-send-response "200-OK" '() output-port
			      (open-input-string (feedback-html)) 0)))
   (POST (lambda (request remainder input-port output-port)
	   (let ((form-data (parse-x-www-form-urlencoded input-port)))
	     (pretty-print form-data)
	     (store-feedback form-data))	   	   
	   (http-send-response "200-OK" '() output-port
			       (open-input-bytes "Post feedback") 0)))))


(define head-links
  (links
   "http://yui.yahooapis.com/2.5.2/build/fonts/fonts-min.css"
   "http://yui.yahooapis.com/2.5.2/build/reset-fonts-grids/reset-fonts-grids.css"
   "http://yui.yahooapis.com/2.5.2/build/base/base-min.css"
   "http://yui.yahooapis.com/2.5.2/build/assets/skins/sam/skin.css"
   "http://yui.yahooapis.com/2.5.2/build/datatable/assets/skins/sam/datatable.css"
   "http://yui.yahooapis.com/2.5.2/build/menu/assets/skins/sam/menu.css"
   "http://yui.yahooapis.com/2.5.2/build/button/assets/skins/sam/button.css"
   "http://yui.yahooapis.com/2.5.2/build/container/assets/skins/sam/container.css"
   "http://yui.yahooapis.com/2.5.2/build/editor/assets/skins/sam/editor.css"
   "../static/knozama/books.css"))

(define head-scripts
  (scripts
   "http://yui.yahooapis.com/2.5.2/build/yahoo-dom-event/yahoo-dom-event.js"
   "http://yui.yahooapis.com/2.5.2/build/dragdrop/dragdrop-min.js"
   "http://yui.yahooapis.com/2.5.2/build/element/element-beta-min.js"
   ;;    "http://yui.yahooapis.com/2.5.2/build/container/container_core.js"    
   ;; example scripts
   "http://yui.yahooapis.com/2.5.2/build/utilities/utilities.js"
   "http://yui.yahooapis.com/2.5.2/build/container/container-min.js"
   "http://yui.yahooapis.com/2.5.2/build/menu/menu-min.js"
   "http://yui.yahooapis.com/2.5.2/build/button/button-min.js"
   "http://yui.yahooapis.com/2.5.2/build/editor/editor-beta-min.js"

   "../static/knozama/mainmenunav.js"))

(define (feedback-html)
  (srl:sxml->html
   `(*TOP*
     (html
      (head
       (title "Knozama")
       
       ,@head-links
       ,@head-scripts
       
       (body (@ (class "yui-skin-sam"))

	     (div (@ (id "doc3") (class "yui-t1"))		  
		  ,knozama-header
		  
		  (div (@ (id "bd"))
		       (div (@ (id "yui-main"))
			    (div (@ (class "yui-b"))
				 
				 (div (@ (id "feedbackbody"))
				      (form (@ (method "post")
					       (action "#") (id "feedbackform"))
					    
					    (fieldset (@ (id "feedback-fieldset"))
						      (div (@ id "name-email")
							   (p 
							    (label (@ (for "feedback-author"))
								   "Name:")
							    (br)
							    (input (@ (id "feedback-author")
								      (value "")(size "30") (name "author"))))
							   (p
							    (label (@ (for "feedback-email"))
								   "Email:") 						   
							    (em "  (not displayed)")
							    (br)
							    (input (@ (id "feedback-email")
								      (value "")(size "30")(name "email")))
							    (br)
							    "Please leave an email if we can contact you for additional information regarding your feedback."))
						      
						      (textarea (@ (id "editor") (name "editor")
								   (rows "20") (cols "120"))
								"Please enter your feedback.")
						      (br)
						      (button (@ (type "button") (id "submitEditor"))
							      "Submit Feedback")))
				      (br)
				      
				      (div (@ (id "status")) ""))))
		       
		       (div (@ (class "yui-b")) ,amazon-left-vertical))
		  
		  
		  (script (@ (src "../static/knozama/feedback.js")) "")
		  
		  ,knozama-footer)
	     
	     ,@ga))))))

