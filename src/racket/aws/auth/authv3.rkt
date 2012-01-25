#lang typed/racket/base

(require
 racket/pretty
 (only-in (planet knozama/common:1/text/util)
	  weave-string-separator)
 (only-in (planet knozama/webkit:1/web/http/header)
	  header->string)
 (only-in (planet knozama/webkit:1/web/uri/url/param)
	  param Param Params))

(: filter-canonicalize-headers (Params -> Params))
(define (filter-canonicalize-headers params)

  (: lower-case-key (Param -> Param))
  (define (lower-case-key param)
    (cons (string-downcase (car param)) (cdr param)))

  (: x-amz-header? (Param -> Boolean))
  (define (x-amz-header? param)
    (let ((k (car param)))
      (and (>= (string-length k) 6)
	   (string=? (substring (car param) 0 6) "x-amz-"))))

  (: merge-value (String String -> String))
  (define (merge-value new-value curr-values)
    (if (string=? curr-values "")
	new-value
	(string-append curr-values "," new-value)))
  
  ;; one-pass loop as opposed to merge-filter-map 3 pass
  (let: ((merged : (HashTable String String) (make-hash)))
    (let: loop : Params ((params : Params params))
      (if (null? params)
	  (hash->list merged)
	  (let ((lparam (lower-case-key (car params))))
	    (when (x-amz-header? lparam)
	      (let ((k (car lparam))
		    (v (cdr lparam)))
		(hash-update! merged k 
			      (lambda: ((curr-value : String))
				(merge-value v curr-value))
			      (lambda () ""))))
	    (loop (cdr params)))))))	      

(: auth-signee (String Params -> String))
(define (auth-signee host params)
  (weave-string-separator "\n" (map header->string (cons (param "host" host) params))))
  
(require
  (only-in (planet knozama/common:1/type/date)
	  current-time-rfc-2822
	  current-time-iso-8601))

(: auth-headers (String -> Params))
(define (auth-headers tok)
  (list 
   (cons "host" "someawshost")
   (cons "x-amZ-security-token" tok)
   (cons "x-amZ-security-token" "tokabc")
   (cons "x-amz-date" (current-time-rfc-2822))
   (cons "x-amz-target" "DynamoDB_20111205.ListTables")))


(pretty-print (auth-signee "rayhost" (filter-canonicalize-headers (auth-headers "tok123"))))

