#lang racket

(provide about-resource)

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

(define about-resource
  (rest-resource
   (GET (lambda (request remainder input-port output-port)
	  (http-send-response "200-OK" '() output-port
			      (open-input-string (about-html))
			      0)))))
(define about-html
  (lambda ()
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

	
	(body (@ (class "yui-skin-sam"))
	      
	      (div (@ (id "doc3") (class "yui-t1"))
		   
		   ,knozama-header
		   
		   (div (@ (id "bd"))
			(div (@ (id "yui-main"))
			     (div (@ (class "yui-b"))
				  (div (@ (class "about-blk"))
				       (h2 (@ (class "about-hl")) "About Knozama")

				       (h3 (@ (class "about-hl")) "Mission")
				       (p  (@ (class "about-p"))
					   "The goal is simple.  I like to read.  I assume you like to read. But it is often difficult to find the next good book to read.  "
					   "There is no one true path to finding a good book, recommendations, friends, reviewers, best sellers, word of mouth, and all "
					   "too often serendipity lead to finding a great book."
					   "The internect has numerous sites devoted to books, and book lovers.  Knozama does not intend to compete with them, instead to leverage "
					   ", not surplant, there individual focus and strenghts with one purpose in mind.  To assist you in finding a good book to read.")
				       
				       (h3 (@ (class "about-hl")) "How we go about it.")					
				       (p  (@ (class "about-p")) 
					   "APIs, APIs, APIs.  WebSites tend to address a particular niche aspect on a particular topic.  "
					   "Increasingly, today's websites are opening up by offering API's.  "
					   "These sites can be stiched together in novel and interesting ways in such a manner where everyone benefits, "
					   "site owners as well as individuals."
					   "The tricky part is how to achieve the desired arrangements so all parties feel benefited by the magnanimity of their APIs.")
				       
				       (h3 (@ (class "about-hl")) "Note to API, RSS, and Content Providers")
				       (p (@ (class "about-p"))
					  "If I am using your API or RSS feed in a manner that you feel violates copyright, usage agreements, etc.  Please just contact me.  "
					  "I will either change the manner in which I use the content in accordance with your wishes or immediately remove it altogether."
					  "Surprisingly many of the posted usage notices are not as clear as you might believe.")

				       (p (@ (class "about-p"))
					  "Contact me directly at ray.racine@gmail.com or leave "
					  (a (@ (href "/feedback")) "feedback."))

				       (h2 (@ (class "about-hl")) "FAQ")
				       (h3 (@ (class "about-hl")) "General")
				       (h3 (@ (class "about-hl")) "Technical")
				       (dl
					(dt (@ (class "faqq")) 
					    (p (em "What technology stack powers the Knozama site?")))
					(dd (@ (class "faqa")) 
					    (p "The site is written in the Scheme programming language (a dialect of Lisp) using "
					       (a (@ (href "http://www.ccs.neu.edu/home/will/Larceny")) 
						  "Larceny Scheme")
					       ".  A custom web framework (Screaming-On-Scheme) was developed and is "
					       (a (@ (href "http://github.com/GreyLensman/rl3/tree/master")) "available") " as open source.")))
				       (dl
					(dt (@ (class "faqq"))
					    (p (em "Why use Scheme and not Ruby, Python, PHP or other dynamic language?")))
					(dd (@ (class "faqa"))
					    (p "Frankly, while Ruby, Python and their accompanying frameworks are " (em "nice") " for developing code and putting together a website, "
					       "they are shadows of Scheme.  "
					       "Scheme, with its powerful macro system, excels in the creation of a domain specific language for the problem at hand.  "
					       "In addition, the correspondance between HTML/XML and Scheme's inate natural list maninpulating facilities provides a powerful templating system.")))					  
				       )))
			
			(div (@ (class "yui-b"))
			     ,amazon-left-vertical))
		   
		   ,knozama-footer)
	      
	      
	      ;; use DOM ready event so I can put this back atop in the headers, deferred as well
	      ,@ga))))))

