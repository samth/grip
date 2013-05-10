;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Ray Racine's TR Library
;; Copyright (C) 2007-2013  Raymond Paul Racine
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#lang typed/racket/base

(provide 
 (struct-out OAuth))

(provide:
 [oauth-authorization-header (OAuth Method Uri -> Param)])

(require 
 racket/pretty
 (only-in prelude/text/util
          weave-string-separator)
 (only-in prelude/std/opt
	  opt-map-orelse-value
 	  opt-map-orelse)
 (only-in crypto/hmac
	  hmac-sha1)
 (only-in crypto/base64
	  base64-encode)
 (only-in httpclient/uri
	  Authority-host
	  Uri Uri-scheme Uri-authority Uri-path Uri-query Uri-fragment
	  make-uri*)
 (only-in httpclient/uri/url/url
	  url-query-params)
 (only-in httpclient/uri/url/param
	  Param Params 
	  param make-params empty-params
	  param-keyval
	  param->encoded-query-kv
	  params->query add-param)
 (only-in httpclient/uri/url/encode	  
	  url-encode-string)
 (only-in httpclient/http/http11
	  Method http-method->string))

(define oauth-version-key	   "oauth_version")
(define oauth-nonce-key		   "oauth_nonce")
(define oauth-timestamp-key	   "oauth_timestamp")
(define oauth-consumer-key-key	   "oauth_consumer_key")
(define oauth-signature-method-key "oauth_signature_method")
(define oauth-signature-key	    "oauth_signature")

(define oauth-version-val "1.0")

(define-type SignatureMethod (U 'HMAC-SHA1 'PLAINTEXT 'RSA-SHA1))

(struct: OAuth ([consumer-key : String]
		[consumer-secret : String]
		[signature-method : SignatureMethod]))

(: generate-nonce (-> String))
(define (generate-nonce)
  (weave-string-separator "" (for/list ([i (in-range 10)])
				    (number->string (random 10)))))

(: current-timestamp (-> String))
(define (current-timestamp)
  (number->string (current-seconds)))

(: signature-method->string (SignatureMethod -> String))
(define (signature-method->string method)
  (symbol->string method))

(: canonicalize-param-order (Params -> Params))
(define (canonicalize-param-order params)
  (: key-lexical (Param Param -> Boolean))
  (define (key-lexical p1 p2)
    (let-values (((p1k  p1v) (param-keyval p1))
		 ((p2k  p2v) (param-keyval p2)))
      (if (string=? p1k p2k)
	  (string<? p1v p2v)
	  (string<? p1k p2k))))
  (sort params key-lexical))

(: build-base-params (OAuth String String -> Params))
(define (build-base-params oauth nonce ts)
  (make-params
   (param oauth-consumer-key-key     (OAuth-consumer-key oauth))
   (param oauth-signature-method-key (signature-method->string (OAuth-signature-method oauth)))
   (param oauth-timestamp-key ts)
   (param oauth-nonce-key nonce)
   (param oauth-version-key oauth-version-val)))

(: build-oauth-params (OAuth Uri String String -> Params))
(define (build-oauth-params oauth url nonce ts)
  (let ((url-query-params (url-query-params url))
	(oauth-base-params (build-base-params oauth nonce ts)))
    (canonicalize-param-order (append url-query-params oauth-base-params))))

(: build-signee (Method Uri Params -> String))
(define (build-signee method url base-params)   
  (weave-string-separator "&" (list (http-method->string method)
				    (url-encode-string (string-append
							(Uri-scheme url)
							"://"
							(let ((auth (Uri-authority url)))
							  (opt-map-orelse-value auth Authority-host ""))
							(Uri-path url)) 
						       #f)
				    (url-encode-string (params->query base-params) #f))))

(: signor (String String -> String))
(define (signor signee key)
  (displayln (format "Signing: ~s" signee))
  (base64-encode (hmac-sha1 (string-append key "&") signee)))

(: build-oauth-signature-params (OAuth Method Uri -> Params))
(define (build-oauth-signature-params oauth method url)
  (let ((nonce (generate-nonce))
	(ts    (current-timestamp)))
    (let ((oauth-params (build-oauth-params oauth url nonce ts)))
      (canonicalize-param-order
       (add-param (param oauth-signature-key (signor (build-signee method url oauth-params)
						     (OAuth-consumer-secret oauth)))
		  oauth-params)))))

(: oauth-authorization-header (OAuth Method Uri -> Param))
(define (oauth-authorization-header oauth method url)
  (let ((auth-str (weave-string-separator "," (map (λ: ((p : Param)) 
						       (param->encoded-query-kv p #t))
						   (build-oauth-signature-params oauth method url)))))
    (param "Authorization" (string-append "OAuth " auth-str))))
