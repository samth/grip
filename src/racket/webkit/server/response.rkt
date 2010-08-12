#lang racket

 (provide BAD-REQUEST-400
	  UNAUTHORIZED
	  FORBIDDEN
	  REQUEST-URI-TOO-LONG
	  SERVICE-UNAVAILABLE-501)
	  
 
;;; 400 Response Codes
 (define BAD-REQUEST-400         "400")
 (define UNAUTHORIZED            "401")
 (define FORBIDDEN               "403")
 (define REQUEST-URI-TOO-LONG    "414")

;;; 500 Response Codes
 (define SERVICE-UNAVAILABLE-501 "501")
