#| Standard GUI Template Elements |#

#lang racket


(provide knozama-header
	 knozama-footer
	 amazon-left-vertical
	 ga urchin)

(define knozama-header
  '(div (* (id "hd"))
	(div (* (class "header"))
	     (div (* (class "nav"))
		  (a (* (href "/about")) "About")
		  " | " (a (* (href "/books")) "Search") 
		  " | " (a (* (href "/cart"))  "Cart")
		  ;;		   " | " (a (* (href "/books")) "Blog") 
		  " | " (a (* (href "/feedback")) "Feedback"))
	     (h1 (a (* (style "float: left") 
		       (href "/books"))
		    "knozama.com"))
	     (h1 (* (class "headertitle"))
		 "Find A Good Book To Read")

	     (div (* (id "mainnavmenu")
		     (class "yuimenubar yuimenubarnav"))
		  (div (* (class "bd"))
		       (ul (* (class "first-of-type"))
			   (li (* (class "yuimenubaritem first-of-type"))
			       (a (* (class "yuimenubaritemlabel")
				     (href "/books")) "Search"))
			   
			   (li (* (class "yuimenubaritem"))
			       (a (* (class "yuimenubaritemlabel")
				     (href "/cart"))"Cart"))
			   
			   (li (* (class "yuimenubaritem"))
			       (a (* (class "yuimenubaritemlabel")
				     (href "/")) "Social"))
			   
			   (li (* (class "yuimenubaritem"))
			       (a (* (class "yuimenubaritemlabel")
				     (href "/reviews"))
				  "Book News")
			       (div (* (id "booknews") (class "yuimenu"))
				    (div (* (class "bd"))					      
					 (ul (li (* (class "yuimenitem"))
						 (a (* (class "yuimenitemlabel")
						       (href "/tours"))
						    "Author Tours"))
					     (li (* (class "yuimenitem"))
						 (a (* (class "yuimenuitemlabel")
						       (href "/reviews"))
						    "Reviews"))))))
			   
			   (li (* (class "yuimenubaritem"))
			       (a (* (class "yuimenubaritemlabel") (href "/awards"))
				  "Book Info")
			       (div (* (id "information") (class "yuimenu"))
				    (div (* (class "bd"))
					 (ul (li (* (class "yuimenuitem"))
						 (a (* (class "yuimenuitemlabel")
						       (href "/awards"))
						    "Award Winners"))))))))))))


(define knozama-footer
  '(div (* (id "ft"))
	(div (* (class "footer"))
	     (div (* (class "nav"))
		  (h1 "")
		  "Impressions and thoughts? " 
		  (a (* (href "/feedback")) "Send us your feedback.")))))


;; deprecated old style.
;; use ga below
(define urchin  
  '((script (* (src "http://www.google-analytics.com/urchin.js")
	       (type "text/javascript")) "")
    (script (* (type "text/javascript"))	     
	    "_uacct = \"UA-4914208-1\";"
	    " urchinTracker();")))

(define amazon-left-vertical
  '(script (* (language "Javascript1.1")
	      (type "text/javascript"))
	   "document.write('<iframe width=\"160\" height=\"600\" src=\"http://apn.amazon.com/gp/apn/hybrid/160x600/?title=' + document.title + '&k=V20070822/US/bravbook-20/8100/1a65fd29-c9a4-48aa-81e2-c3f49933c67f\" frameborder=\"no\" scrolling=\"no\" marginheight=\"0\" marginwidth=\"0\"></iframe>');"))


;; Google Analytics.
(define ga
  '((script (* (type "text/javascript"))
	    "var gaJsHost = ((\"https:\" == document.location.protocol) ? \"https://ssl.\" : \"http://www.\");"
	    "document.write(unescape(\"%3Cscript src='\" + gaJsHost + \"google-analytics.com/ga.js' type='text/javascript'%3E%3C/script%3E\"));")
    (script (* (type "text/javascript"))
	    "var pageTracker = _gat._getTracker(\"UA-4914208-1\");"
	    "pageTracker._initData();"
	    "pageTracker._trackPageview();")))

