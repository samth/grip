#lang typed/racket/base

(require
 racket/pretty
 (only-in (planet knozama/webkit:1/web/uri)
	  Uri make-uri uri->string)
 (only-in (planet knozama/webkit:1/web/http/http11)
	  HTTPConnection-in http-successful? http-close-connection http-invoke)
 (only-in (planet knozama/xml:1/sxml)
	  Sxml SXPath sxpath xml->sxml extract-text extract-integer)
 (only-in "config.rkt"
	  sts-host)
 (only-in "session.rkt"
	  SessionCredential STSError))

;; https://sts.amazonaws.com/
;; ?Version=2011-06-15
;; &Action=GetSessionToken
;; &DurationSeconds=3600
;; &AUTHPARAMS

(: per-request-params ((Option String) String -> (Listof Param)))
(define (pre-request-params domain action)
  (append (sts-std-params (Aws-Credential-access-key (current-aws-credential)))
	  (let ((ps (list (cons "Action" action)
			(cons "Timestamp" (url-encode-string (current-time-iso-8601) #f)))))
	    (if domain 
	       (cons (cons "DomainName" domain) ps)
	       ps))))

(: invoke-uri (String String -> Uri))
(define (invoke-uri path query)
  (make-uri "https" #f sts-host 443 path query ""))

(: invoke-sts-get (All (a) (Uri (Listof String) (Sxml -> (U STSError a)) -> (U STSError a))))
(define (invoke-sts-get url headers resp-parser)
  (with-handlers ([exn:fail? 
		   (lambda (ex) (STSError))])
    (let ((conn (http-invoke 'GET url headers #f)))
      (pretty-print conn)
      (let ((page (xml->sxml (HTTPConnection-in conn) '())))
	(pretty-print page)
	(resp-parser page)))))

;; 3600s (one hour) to 129600s (36 hours), with 43200s (12 hours) as default
(: get-session-token (Natural -> SessionCredential))
(define (get-session-token duration-secs)
  (assert (and (>= duration-secs 3600) (<= duration-secs 129600)))
  (SessionCredential "" "" "" ""))
  
  
