#lang racket

(provide main)

(require
 (only-in knozama/web/uri
	  authority-host)  
 (only-in knozama/web/http/proxy
	  add-proxy-proc!
	  set-http-proxy!)
 (only-in knozama/web/http/http	
	  http-301-moved-permanently)
 (only-in knozama/web/server/server
	  web-server)
 (only-in knozama/web/server/dispatch
	  rest-resource)
 (only-in knozama/web/server/static
	  serve-static-resource
	  static-resource)
 (only-in knozama/site/configure
	  PORT)
 (only-in knozama/site/books/books
	  books-resource)
 (only-in knozama/site/books/book
	  book-resource)
 (only-in knozama/site/carts/cart
	  cart-resource)
 (only-in knozama/site/tours/tours
	  tours-resource)
 (only-in knozama/site/feedback
	  feedback-resource)
 (only-in knozama/site/reviews/reviews
	  reviews-resource)
 (only-in knozama/site/awards/awards
	  awards-resource)
 (only-in knozama/site/about
	  about-resource))

(define redirect-resource
  (rest-resource
   (GET (lambda (request remainder input-port output-port)
	  (http-301-moved-permanently "/books" output-port)))))

(define root-resource
  (rest-resource
   (GET (lambda (request remainder input-port output-port)
	  (let ((path-remainder (car remainder)))
	    (if (and (eqv? (length path-remainder) 1)
		  (string=? (car path-remainder) ""))
	       (books-resource request remainder input-port output-port)
	       (serve-static-resource path-remainder output-port)))))))

(define dispatch-configure
   (lambda ()
     `((""          ,root-resource
	("static"   ,static-resource)
	("books"    ,books-resource
	 ("book"    ,book-resource))
	("cart"     ,cart-resource)
	("reviews"  ,reviews-resource)
	("tours"    ,tours-resource)
	("awards"   ,awards-resource)
	("feedback" ,feedback-resource)
	("about"    ,about-resource)))))

(define proxy-escape
  (lambda (symbol host)
    (add-proxy-proc! symbol (lambda (authority uri)
			      (string=? (authority-host authority) host)))))

(define main
  (lambda ()
    (set-http-proxy! "127.0.0.1" 80)
    (proxy-escape 'aws "ecs.amazonaws.com")
    (proxy-escape 'booktour "booktour.com")
    (proxy-escape 'blippr "blippr.com")
    (proxy-escape 'feedproxy "feedproxy.feedburner.com")		       
    (web-server dispatch-configure PORT)))
