#lang racket

(provide bookgrid-resource)

(require
 (only-in (planet lizorkin/sxml:2:1/sxml)
	  sxpath sxml:text srl:sxml->html)
 (only-in knozama/web/http/http
	  http-send-response)
 (only-in knozama/web/http/cookies
	  make-cookie parse-cookie)
 (only-in knozama/web/http/headers
	  get-cookie-header SET-COOKIE HOST)
 (only-in knozama/web/http/encoding
	  parse-x-www-form-urlencoded)
 (only-in knozama/web/server/log
	  www-log)
 (only-in knozama/web/server/dispatch
	  rest-resource)
 (only-in knozama/web/html/html
	  stylesheets scripts)
 (only-in knozama/site/templates
	  ga))

(define bookgrid-resource
  (rest-resource
   (GET (lambda (request remainder inp outp)
	  (let ((content (srl:sxml->html (bookgrid-html))))
	    (http-send-response "200 OK" '() outp (open-input-string content) 0))))))

(define head-stylesheets
  (stylesheets
   ;; CSS : implied media="all"
   "/static/css/style.css?v=1"
   ;; For the less-enabled mobile browsers like Opera Mini
   "/static/css/handheld.css?v=1"
   "/static/knozama/books.css"))

(define head-scripts
  (scripts
  ;;All JavaScript at the bottom, except for Modernizr which enables HTML5 elements & feature detects
   "/static/js/modernizr-1.5.min.js"))

(define bookgrid-html
  (lambda ()
    `(*TOP*
      (html
       
       (head
	(meta (@ (charset "utf-8")))
	(title "Knozma")
	(meta (@ (name "description")
		 (content "Find Good Book To Read")))
	(meta (@ (name "author")
		 (content "Knozama, Ray Racine")))
	(meta (@ (name "viewport")
		 (content "width=device-width; initial-scale=1.0; maximum-scale=1.0")))
	,@head-stylesheets
	,@head-scripts)

       (body 
	(div (@ (id "container"))
	     (header
	      (h1 (a (@ (style "float: left")
			(href "/books"))
		     "Knozama"))
	      (h1 (@ (class "headertitle"))
		  "Find A Good Book To Read"))
	     (div (@ (id "main"))
		  (h1 "Find a book"))
	     (footer 
	      (h3 "Footer goes here"))
	,@(scripts
	  "http://ajax.googleapis.com/ajax/libs/jquery/1.4.2/jquery.min.js"
	  "/static/js/plugins.js?v=1"
	  "/static/js/script.js?v=1"
	  ;; profiling
	  "/static/js/profiling/yahoo-profiling.min.js?v=1"
	  "/static/js/profiling/config.js?v=1")


	(script 
	 "var _gaq = [['_setAccount', 'UA-4914208-1'], ['_trackPageview']];"
	 "(function(d, t) {"
	 "var g = d.createElement(t),"
	 "s = d.getElementsByTagName(t)[0];"
	 "g.async = true;"
	 "g.src = '//www.google-analytics.com/ga.js';"
	 "s.parentNode.insertBefore(g, s);"
	 "})(document, 'script');")))))))





