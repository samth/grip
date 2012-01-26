#lang typed/racket/base

(provide 
 get-session-token)

(require
 racket/pretty
 (only-in (planet knozama/webkit:1/web/uri)
	  Uri make-uri uri->string)
 (only-in (planet knozama/webkit:1/web/http/http11)
	  HTTPConnection-in http-successful? http-close-connection http-invoke)
 (only-in (planet knozama/webkit:1/web/uri/url/param)
	  Param Params params->query)
 (only-in (planet knozama/xml:1/sxml)
	  Sxml SXPath sxpath xml->sxml extract-text extract-integer)
 (only-in (planet knozama/webkit:1/web/http/header)
          make-header-string)
 (only-in "../credential.rkt"
	  current-aws-credential AwsCredential AwsCredential? AwsCredential-session 
	  SessionCredential SessionCredential? SessionCredential-expiration)
 (only-in "../auth/authv2.rkt"
	  authv2-signature)
 (only-in "config.rkt"
	  get-session-token-action sts-host sts-api-version)
 (only-in "error.rkt"
	  STSError)
 (only-in "session.rkt"
	  parse-session-response))

(: request-headers (Listof String))
(define request-headers  
  (list 
   ;; (make-header-string "User-Agent" "Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/535.11 (KHTML, like Gecko) Chrome/17.0.963.2 Safari/535.11")
   (make-header-string "User-Agent" "Googlebot/2.1 (+http://www.google.com/bot.html)")
   (make-header-string "Accept" "text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8")
   (make-header-string "Accept-Charset" "ISO-8859-1,utf-8;q=0.7,*;q=0.3")
   (make-header-string "Accept-Encoding" "gzip")
   (make-header-string "Accept-Language" "en-US,en;q=0.8")
   (make-header-string "Cache-Control" "max-age=0")
   (make-header-string "Connection" "Close")))

(: invoke-uri (String String -> Uri))
(define (invoke-uri path query)
  (make-uri "https" #f sts-host 443 path query ""))

(: invoke-sts-get (All (a) (Uri (Listof String) (Sxml -> (U STSError a)) -> (U STSError a))))
(define (invoke-sts-get url headers resp-parser)
  (with-handlers ([exn:fail?
		   (lambda (ex) 
		     (pretty-print ex)
		     (STSError))])
    (let ((conn (http-invoke 'GET url headers #f)))
      (let ((page (xml->sxml (HTTPConnection-in conn) '())))
	(http-close-connection conn)
	(resp-parser page)))))

(: signed-query (String (Listof (Pairof String String)) -> Params))
(define (signed-query cmd qparams)
  (authv2-signature sts-api-version "GET" sts-host cmd "/" qparams))

(: duration-param (Natural -> Param))
(define (duration-param duration-secs)  
  ;; (assert (and (>= duration-secs 3600) (<= duration-secs 129600)))
  (cons "DurationSeconds" (number->string duration-secs)))

;; 3600s (one hour) to 129600s (36 hours), with 43200s (12 hours) as default
(: get-session-token (Natural -> (U STSError AwsCredential)))
(define (get-session-token duration-secs)
  (let ((url (invoke-uri "/" (params->query (signed-query get-session-token-action '())))))
    (pretty-print (uri->string url))
    (invoke-sts-get url request-headers parse-session-response)))

(: expired-token? (SessionCredential -> Boolean))
(define (expired-token? creds)
  (let ((expiry (SessionCredential-expiration creds)))
    (if expiry #t #f)))

(: refresh-token (-> Boolean))
(define (refresh-token)
  (let ((tok (get-session-token 100)))
    (if (AwsCredential? tok)
	#t #f)))

(: ensure-session (-> Boolean))
(define (ensure-session)
  (if (expired-token? (current-aws-credential))
    (refresh-token)
    #t))
