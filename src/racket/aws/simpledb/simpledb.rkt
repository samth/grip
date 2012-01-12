#lang typed/racket/base

(require
 racket/pretty
 (only-in (planet knozama/common:1/std/control)
	  aif)
 (only-in (planet knozama/aws:1/auth)
	  sdb-auth-str sdb-auth-mac-encode)
 (only-in (planet knozama/aws:1/credential)
	  Aws-Credential-secret-key Aws-Credential-access-key current-aws-credential)
 (only-in (planet knozama/webkit:1/web/uri/url/param)
	  params->query parse-params)
 (only-in (planet knozama/webkit:1/crypto/base64)
	  base64-encode)
 (only-in (planet knozama/webkit:1/crypto/hmac)
	  hmac-sha256)
 (only-in (planet knozama/webkit:1/web/uri)
	  Uri Uri-query make-uri parse-uri
	  url-encode-string uri->string)
 (only-in (planet knozama/common:1/type/date)
	  current-time-iso-8601)
 (only-in (planet knozama/webkit:1/web/http/http11)
	  HTTPConnection-in http-successful? http-close-connection http-invoke)
 (only-in (planet knozama/webkit:1/web/http/header)
          make-header-string)
 (only-in (planet knozama/xml:1/sxml)
	  Sxml SXPath sxpath html->sxml xml->sxml extract-text extract-integer)
 (only-in "../configuration.rkt"
	  sdb-ns sdb-std-params))

(: CREATE-ACTION String)
(define CREATE-ACTION "CreateDomain")

(: DELETE-ACTION String)
(define DELETE-ACTION "DeleteDomain")

(: LIST-ACTION String)
(define LIST-ACTION "ListDomains")

(: META-ACTION String)
(define META-ACTION "DomainMetadata")

(: PUT-ACTION String)
(define PUT-ACTION "PutAttributes")

(: request-signature (String -> String))
(define (request-signature signee)
  (url-encode-string (base64-encode (hmac-sha256 (Aws-Credential-secret-key (current-aws-credential)) signee)) #f))

(: request-headers (Listof String))
(define request-headers  
  (list 
   ;; (make-header-string "User-Agent" "Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/535.11 (KHTML, like Gecko) Chrome/17.0.963.2 Safari/535.11")
   (make-header-string "User-Agent" "Googlebot/2.1 (+http://www.google.com/bot.html)")
   (make-header-string "Accept" "text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8")
   (make-header-string "Accept-Charset" "ISO-8859-1,utf-8;q=0.7,*;q=0.3")
   ;;(make-header-string "Accept-Encoding" "gzip,deflate,sdch")
   (make-header-string "Accept-Language" "en-US,en;q=0.8")
   (make-header-string "Cache-Control" "max-age=0")
   (make-header-string "Connection" "Close")))

;; Create the ("Signature" . "[signed request signature]")
(: signature-param (String String (Listof (Pairof String String)) -> (Pairof String String)))
(define (signature-param action path qparams)
  (cons "Signature" (request-signature (sdb-auth-str action path qparams))))

(: create-ephemeral-params ((Option String) String -> (Listof (Pairof String String))))
(define (create-ephemeral-params domain action)
  (append (sdb-std-params (Aws-Credential-access-key (current-aws-credential)))
	  (let ((ps (list (cons "Action" action)
			(cons "Timestamp" (url-encode-string (current-time-iso-8601) #f)))))
	    (if domain 
	       (cons (cons "DomainName" domain) ps)
	       ps))))

(struct: SDBError () #:transparent)

(struct: ListDomains ([domains  : (Listof String)]
		      [nextToken : (Option String)]) #:transparent)

(struct: MetaDomain ([item-count : Integer]
		     [item-size : Integer]
		     [name-count : Integer]
		     [name-size : Integer]		     
		     [value-count : Integer]
		     [value-size : Integer]
		     [timestamp : Integer]) #:transparent)

(: invoke-uri (String String -> Uri))
(define (invoke-uri path query)
  (make-uri "https" #f "sdb.amazonaws.com" 443 path query ""))

(: invoke-signed-query (String String (Listof (Pairof String String)) -> String))
(define (invoke-signed-query action path qparams)
  (params->query (cons (signature-param action path qparams) qparams)))

(: invoke-sdb-get (All (a) (Uri (Listof String) (Sxml -> (U SDBError a)) -> (U SDBError a))))
(define (invoke-sdb-get url headers resp-parser)
  (with-handlers ([exn:fail? 
		   (lambda (ex) (SDBError))])
    (let ((conn (http-invoke 'GET url headers #f)))
      (let ((page (xml->sxml (HTTPConnection-in conn) '())))
	(pretty-print page)
	(resp-parser page)))))

(: parse-list-domains-resp (Sxml -> (U SDBError ListDomains)))
(define (parse-list-domains-resp sxml)
  (ListDomains '() ""))

(: list-domains ((Option Integer) -> (U SDBError ListDomains)))
(define (list-domains maxList)
  (let ((qparams (create-ephemeral-params #f LIST-ACTION)))
    (let ((url (invoke-uri "/" (invoke-signed-query "GET" "/" qparams))))
      (invoke-sdb-get url request-headers parse-list-domains-resp))))

(: parse-create-domain-resp (Sxml -> (U SDBError True)))
(define (parse-create-domain-resp sxml)
  #t)

(: create-domain (String -> (U SDBError True)))
(define (create-domain domain)
  (let ((qparams (create-ephemeral-params domain CREATE-ACTION)))
    (let ((url (invoke-uri "/" (invoke-signed-query "GET" "/" qparams))))
      (invoke-sdb-get url request-headers parse-create-domain-resp))))

(: parse-delete-domain-resp (Sxml -> (U SDBError True)))
(define (parse-delete-domain-resp sxml)
  #t)

(: delete-domain (String -> (U SDBError True)))
(define (delete-domain domain)
  (let ((qparams (create-ephemeral-params domain DELETE-ACTION)))
    (let ((url (invoke-uri "/" (invoke-signed-query "GET" "/" qparams))))
      (invoke-sdb-get url request-headers parse-delete-domain-resp))))

(: mk-sxpath (String -> SXPath))
(define (mk-sxpath path)
  (sxpath path `(,sdb-ns)))

(define sx-meta-result		(mk-sxpath "/sdb:DomainMetadataResponse/sdb:DomainMetadataResult"))
(define sx-meta-item-count		(mk-sxpath "/sdb:ItemCount/text()"))
(define sx-meta-item-names-size	(mk-sxpath "/sdb:ItemNamesSizeBytes/text()"))
(define sx-meta-attr-names-count	(mk-sxpath "/sdb:AttributeNameCount/text()"))
(define sx-meta-attr-names-size	(mk-sxpath "/sdb:AttributeNamesSizeBytes/text()"))
(define sx-meta-attr-values-count	(mk-sxpath "/sdb:AttributeValueCount/text()"))
(define sx-meta-attr-values-size	(mk-sxpath "/sdb:AttributeValuesSizeBytes/text()"))
(define sx-meta-timestamp		(mk-sxpath "/sdb:Timestamp/text()"))

(: parse-meta-domain-resp (Sxml -> (U SDBError MetaDomain)))
(define (parse-meta-domain-resp sxml)
  
  (: extract-int-minus-error (Sxml -> Integer))
  (define (extract-int-minus-error sxml)
    (aif (extract-integer sxml) it -1))
  
  (let ((result (sx-meta-result sxml)))
    (let ((item-cnt (extract-int-minus-error (sx-meta-item-count result)))
	(item-names-sz (extract-int-minus-error (sx-meta-item-names-size result)))
	(attr-name-cnt (extract-int-minus-error (sx-meta-attr-names-count result)))
	(attr-names-sz (extract-int-minus-error (sx-meta-attr-names-size result)))
	(attr-values-cnt (extract-int-minus-error (sx-meta-attr-values-count result)))
	(attr-values-sz (extract-int-minus-error (sx-meta-attr-values-size result)))
	(ts (extract-int-minus-error (sx-meta-timestamp result))))
      (MetaDomain item-cnt item-names-sz 
		  attr-name-cnt attr-names-sz
		  attr-values-cnt attr-values-sz ts))))

(: meta-domain (String -> (U SDBError MetaDomain)))
(define (meta-domain domain)
  (let ((qparams (create-ephemeral-params domain META-ACTION)))
    (let ((url (invoke-uri "/" (invoke-signed-query "GET" "/" qparams))))
      (invoke-sdb-get url request-headers parse-meta-domain-resp))))

(struct: Attr ([name : String] [value : String] [replace : Boolean]) #:transparent)

(: put-attributes ((Listof Attr) -> (U SDBError True)))
(define (put-attributes attrs)
  ( ... ))
