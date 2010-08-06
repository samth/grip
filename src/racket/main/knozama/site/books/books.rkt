#lang racket

(provide books-resource)

(require
 (only-in (planet lizorkin/sxml:2:1/sxml)
	  sxpath sxml:text srl:sxml->html)
 (only-in knozama/type/list
	  assoc-value)
 (only-in knozama/std/control
	  aif)
 (only-in knozama/site/configure
	  aws-creds)
 (only-in knozama/aws/configuration
	  a2s-nss a2s-ns)
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
	  links scripts)
 (only-in knozama/site/templates
	  ga
	  knozama-header
	  knozama-footer
	  amazon-left-vertical)
 (only-in knozama/site/books/itemimages
	  extract-item-imageset)
 (only-in knozama/site/books/itemattrs
	  item-attributes 
	  item-attrs-group
	  item-attrs-title
	  item-attrs-authors 
	  item-attrs-manufacturer
	  extract-item-attrs)
 (only-in knozama/site/books/utils
	  select-single-node-text)
 (only-in knozama/aws/a2s/search
	  keyword-search))

;; (only (rl3 xml sxml serializer)
;; 	sxml->html)
;; (only (rl3 web httpserver dispatch)
;; 	rest-resource)
;; (only (rl3 web http http)
;; 	http-send-response)
;; (only (rl3 web http encoding)
;; 	parse-x-www-form-urlencoded)
;; (only (rl3 xml sxml sxpath)
;; 	sxpath)
;; (only (rl3 xml sxml sxml-tools)
;; 	sxml:text)
;; (only (rl3 json json-writer)
;; 	json-write)
;; (only (rl3 aws a2s a2s)
;; 	keyword-search)
;; (only (rl3 types strings)
;; 	string-tokenize)
;; (only (rl3 types chars)
;; 	char-set:symbol char-set-complement)
;; (only (rl3 text text)
;; 	weave-string-separator)
;; (only (rl3 web httpserver log)
;; 	www-log)
;; (only (rl3 web uri)
;; 	url-encode-string)
;; (only (rl3 web http headers)
;; 	SET-COOKIE
;; 	get-cookie-header)
;; (only (rl3 web http cookies)
;; 	make-cookie
;; 	parse-cookie)
;; (only (rl3 web html html)
;; 	scripts links)
;; (only (knozama configure)
;; 	HOST
;; 	aws-creds)
;; (only (knozama templates)
;; 	amazon-left-vertical
;; 	ga knozama-header knozama-footer)
;; ;;  (only (knozama books reviews)
;; ;;	extract-customer-reviews)
;; (only (rl3 aws configuration)
;; 	a2s-ns)
;; (only (knozama books itemattrs)
;; 	extract-item-attrs
;; 	item-attrs-manufacturer
;; 	item-attrs-group
;; 	item-attrs-authors
;; 	item-attrs-title
;; 	item-attributes)
;; (only (knozama books itemimages)
;; 	extract-item-imageset)
;; (primitives open-output-string get-output-string))

;; AWS search index to use (kindle or just books).
(define search-index-cookie-key "si")

(define search-terms-cookie-key "st")

(define default-search-terms "books")

(define nss (list a2s-ns))

;; remainder -> #f or "terms=" or "terms=a+b+c..."
;; returns default-search-terms | not null? listof terms.  
;; the cdr drops the first "terms"
(define parse-query-form
  (lambda (query)
    (if query
       (let ((form (parse-x-www-form-urlencoded (open-input-string query))))
	 (if (assoc-value "terms" form)
	    form
	    (cons (cons "terms" default-search-terms) form)))
       (list (cons "terms" "")))))

(define sx-items
  (sxpath "/a2s:ItemSearchResponse/a2s:Items/a2s:Item" nss))

(define item-asin
  (let ((sxp (sxpath "/a2s:ASIN" nss)))
    (lambda (nodelst)
      (sxml:text (sxp nodelst)))))

(define item-url
  (select-single-node-text "/a2s:DetailPageURL" nss))

(define sales-rank
  (select-single-node-text "/a2s:SalesRank" nss))

;;  ;; FIX ME RPR
(define item-image
  (lambda (item-sxml)
    (assoc-value 'tiny (extract-item-imageset item-sxml))))

;;FIXME RPR - multiple authors.  See itemattrs.sls
(define item-attr-author
  (select-single-node-text "/a2s:Author" nss))

;; search results into an alist
;; alist can be used by json-write
(define item-row-from-item
  (lambda (item)
    (let ((asin-value (item-asin item)))
      (let ((url  (item-url  item))  ;; this is amazon url USE IT RPR!
	  (rank (string->number (sales-rank item)))
	  (item-image (item-image item)))
	(let ((asin (string-append "<a href=" url ">" asin-value "</a>")))
	  (let ((attr (item-attributes item)))
	    ;; FIX ME RPR - What about multiple authors???
	    (let ((author (let ((authors (item-attrs-authors attr)))
			  (if (pair? authors)
			     (car authors)
			     "Unknown")))
		(manuf  (item-attrs-manufacturer attr))
		(group  (item-attrs-group attr))
		(title-val (item-attrs-title attr)))
	      (let ((knozama-url (string-append "/books/book/" asin-value)))
		`(div (* (class "booklistitem"))
		      (image (* (class "booklistimage")
				(width "72px")
				(height "110px")
				(src ,item-image)) "")
		      (span (* (class "booklistitemcontent"))
			    (span (* (class "booklistitemtitle"))
				  (a (* (class "booklistitemtitle")
					(href ,knozama-url)) ,title-val))
			    (br) " by "
			    (span (* (class "booklistitemauthor")),author)
			    " Sales Rank: "
			    ,rank))))))))))

(define empty-search
  '())

(define extract-items-data-table
  (lambda (items)
    (if (null? items)	
       empty-search
       (map item-row-from-item items))))

(define books-content
  (lambda (kindle-index terms)
    ;; (pretty-print "KEYWORD")
    ;; (let ((search-results (keyword-search aws-creds (if kindle-index 'KINDLE 'BOOK) terms)))
    ;;   (pretty-print search-results)
    ;;   (let ((items (sx-items search-results)))
    ;; 	(let ((item-rows (extract-items-data-table items)))
    ;; 	  item-rows)))))
    empty-search))

;; is kindle search index specified in the 'si=k' cookie?
(define (cookie-kindle-search-index? cookies)
  (aif (assoc-value search-index-cookie-key cookies)
       (string=? it "k")
       #f))

;; is kindle search index specified in the form checkbox 'kindlecb'?
(define (form-kindle-search-index? form)
  (aif (assoc-value "kindlecb" form)
       (string=? it "k")
       #f))

(define expiry (* 60 60 24 10))

(define (make-search-index-cookie-header index-symbol)
  (cons SET-COOKIE (make-cookie HOST "/" search-index-cookie-key
				(case index-symbol
				  ((KINDLE) "k")
				  ((BOOK)   "b")
				  (else "b"))
				expiry)))

(define books-resource
  (rest-resource
   (GET (lambda (request remainder input-port output-port)
	  (let ((query (cadr remainder)))
	    (let ((form (parse-query-form query)))
	      (let ((cookies (parse-cookie (get-cookie-header (cdr request)))))		 
		(let ((cookie-kindle (cookie-kindle-search-index? cookies))
		    (form-kindle (form-kindle-search-index? form)))
		  (let ((terms (assoc-value "terms" form)))
		    (let ((search-terms (if (string=? terms "")
					 "books"
					 terms)))
		      (let ((response-headers (if (not query)
					       '()
					       (if form-kindle
						  (list (make-search-index-cookie-header 'KINDLE))
						  (list (make-search-index-cookie-header 'BOOK))))))
			(let ((kindle-checked (if query form-kindle cookie-kindle)))
			  (let ((datarows (books-content kindle-checked search-terms)))
			    ;; if no query we came in from outside the search form 
			    (displayln (books-html datarows
						   kindle-checked
						   terms))
			    (let ((content (srl:sxml->html (books-html datarows  
								 kindle-checked
								 terms))))
			      (www-log "Sending Response for books-resource GET~%")
			      (http-send-response "200 OK" response-headers output-port
						  (open-input-string content) 0)))))))))))))))

(define head-links
  (links
   "http://yui.yahooapis.com/2.5.2/build/fonts/fonts-min.css"
   "http://yui.yahooapis.com/2.5.2/build/reset-fonts-grids/reset-fonts-grids.css"
   "http://yui.yahooapis.com/2.5.2/build/base/base-min.css"
   "http://yui.yahooapis.com/2.5.2/build/assets/skins/sam/skin.css"
   "http://yui.yahooapis.com/2.5.2/build/menu/assets/skins/sam/menu.css"
   "../static/knozama/books.css"))

(define head-scripts
  (scripts
   "http://yui.yahooapis.com/2.5.2/build/yahoo-dom-event/yahoo-dom-event.js"
   "http://yui.yahooapis.com/2.5.2/build/dragdrop/dragdrop-min.js"
   "http://yui.yahooapis.com/2.5.2/build/element/element-beta-min.js"
   "http://yui.yahooapis.com/2.5.2/build/container/container_core.js"
   "http://yui.yahooapis.com/2.5.2/build/menu/menu.js"
   "http://yui.yahooapis.com/2.5.2/build/button/button-min.js"
   "../static/knozama/mainmenunav.js"))

(define books-html
  (lambda (items kindlecb terms)
    `(*TOP*
      (html
       
       (head
	(title "Knozama.com")
	
	,@head-links
	,@head-scripts
	
	(style (* (type "text/css"))
	  ".yui-skin-sam .yui-dt-liner { white-space:nowrap; } "))
       
       (body (* (class "yui-skin-sam"))
	     (div (* (id "doc3") (class "yui-t1"))
		  
		  ,knozama-header
		  
		  (div (* (id "bd"))
		       
		       (div (* (id "yui-main"))
			    (div (* (class "yui-b"))
				 (div ;;(* (class "yui-gc"))
				  
				  (div (* 
					;;(class "yui-u-first") 
					(style "text-align: center") (id "mainsearch"))
				       (div (* (id "tagline"))
					    (span (strong "knozama.com") " just wants to find you a good book to read."))
				       (h1 "")
				       (form (* (action "/books") (method "GET") (id "searchform"))
					     (h2  "Find a good book to read.")
					     (p (strong "search words ")
						(input (* (type "text")  (name "terms") (style "width: 34%")
							  (value ,terms)))
						(input (* (type "checkbox") (id "checkbox1") (name "kindlecb") (value "k") 
							  ,(if kindlecb '(checked) '())) "Kindle")
						(input (* (type "submit") (value "Search"))))
					     (p (* (class "explain")) 
						(em "Examples:") (a (* (href "/books/?terms=scifi")) "scifi")))
				       
				       (div (* (class "booklist"))
					    ,@items)))))
		       
		       
		       (div (* (class "yui-b"))
			    ,amazon-left-vertical))
		  
		  ,knozama-footer)
	     
	     ,@ga)))))



