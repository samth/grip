;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Knozama's Amazon API Library
;; Copyright (C) 2007-2011  Raymond Paul Racine
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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#lang typed/racket/base

(provide 
 s3-list-buckets
 s3-list-bucket-objects)

(require 
 racket/pretty
 (only-in "../credential.rkt"
	  current-aws-credential
	  Aws-Credential
	  Aws-Credential-access-key
	  Aws-Credential-secret-key)
 (only-in (planet knozama/common:1/type/date)
	  current-time-rfc-2822)
 (only-in (planet knozama/webkit:1/web/http/heading)
	  DATE HOST)
 (only-in (planet knozama/webkit:1/web/http/header)
	  Header
	  header->string
	  make-header)
 (only-in (planet knozama/webkit:1/web/http/http11)
	  http-invoke http-close-connection HTTPConnection-in)
 (only-in (planet knozama/webkit:1/web/uri/url/param)
	  parms->query)
 (only-in (planet knozama/xml:1/sxml)
	  Sxml SXPath 
	  sxpath xml->sxml select-single-node-text)
 (only-in "../auth.rkt"
	  aws-auth-str
	  aws-auth-mac)
 (only-in "configuration.rkt"
	  s3-namespace)
 (only-in "types.rkt" 
	  Owner Bucket Buckets Objects Object))


;; (require 
;;  racket/date
;;  racket/contract
;;  net/url
;;  net/base64
;;  file/md5
;;  "../auth.rkt"
;;  "../credential.rkt"
;;  "types.rkt"
;;  "headers.rkt"
;;  "response.rkt")

;;(lib "xml.ss" "xml")

(require (only-in "../configuration.rkt"
		  s3-host)
	 (only-in (planet knozama/webkit:1/web/uri)
		  make-uri Uri Uri-path uri->string))

(: make-base-uri (String -> (Option Uri)))
(define (make-base-uri path)
  (make-uri "http" #f s3-host #f path #f #f))

(: aws-error (String -> Void))
(define (aws-error s)
  (display s) (newline))

(: authorization-header (Aws-Credential String -> Header))
(define (authorization-header credential auth-str)
  (make-header "Authorization" 
	       (string-append  "AWS " (Aws-Credential-access-key credential) 
			       ":" (aws-auth-mac (Aws-Credential-secret-key credential)
						 auth-str))))

;; (define (make-bucket-url bucket)
;;   (let ((url (make-base-url)))
;;     (set-url-path! url (list bucket))
;;     url))    

(: nss (List (Pair Symbol String)))
(define nss `((s3 . ,s3-namespace)))

(: empty-response (List Symbol))
(define empty-response '(*TOP*))

(: s3-get-invoke (Uri -> (Listof Any)))
(define (s3-get-invoke url)
  (let* ((datetime (current-time-rfc-2822))
       (headers (map header->string 
		     (list (make-header DATE datetime)
			   (authorization-header (current-aws-credential) 
						 (aws-auth-str "GET" "" "" 
							       datetime '() 
							       (Uri-path url)))))))
    (let ((connection (http-invoke 'GET url headers #f)))
      (with-handlers [(exn:fail? (lambda (ex)
				   ((error-display-handler) "ERROR in S3 invocation." ex)
				   (displayln ex) 
				   (http-close-connection connection)
				   empty-response))]
	(let ((results (xml->sxml (HTTPConnection-in connection) '())))
	  (http-close-connection connection)
	  results)))))

(: s3-list-buckets (-> Buckets))
(define (s3-list-buckets)

  (: parse-owner (Sxml -> Owner))
  (define (parse-owner sxml)
    (define sx-id (select-single-node-text "/s3:Owner/s3:ID" nss))
    (define sx-name (select-single-node-text "/s3:Owner/s3:DisplayName" nss))
    (let ((id (sx-id sxml))
	(name (sx-name sxml)))
      (Owner id name)))

  (: parse-bucket (Sxml -> Bucket))
  (define (parse-bucket sxml)
    (define sx-name (select-single-node-text "/s3:Name" nss))
    (define sx-create (select-single-node-text "/s3:CreationDate" nss))
    (let ((name (sx-name sxml))
	(create (sx-create sxml)))
      (Bucket name create)))

  (: parse-buckets (Sxml -> (Listof Bucket)))
  (define (parse-buckets sxml)
    (: sx-buckets SXPath)
    (define sx-buckets (sxpath "/s3:Buckets/s3:Bucket" nss))
    (let ((bs (sx-buckets sxml)))
      (if (andmap list? bs)
	 (map parse-bucket bs)
	 '())))

  (: parse-response (Sxml -> Buckets))
  (define (parse-response sxml)
    (: sx-resp SXPath)
    (define sx-resp (sxpath "/s3:ListAllMyBucketsResult" nss))
    (let ((resp (sx-resp sxml)))
      (if resp
	 (begin
	   (let ((owner (parse-owner resp))
	       (buckets (parse-buckets resp)))
	     (Buckets owner buckets)))
	 (error "S3 call failed"))))

  (let ((url (make-base-uri "/")))
    (parse-response (s3-get-invoke (assert url)))))

;; FIXME Use opt-map to create the parameter query string

;;(: list-bucket-objects (String Keyword String Keyword String Keyword Integer -> (Listof Any)))
;;(define (list-bucket-objects bucket #:prefix prefix #:marker marker #:max max)

(: s3-list-bucket-objects (String String String Integer -> Objects))
(define (s3-list-bucket-objects bucket prefix marker max)

  (: s->i (String -> (Option Integer)))
  (define (s->i s)
    (let ((i (string->number s)))
      (if (exact-integer? i)
	 i
	 #f)))
  (: s->b (String -> Boolean))
  (define (s->b s)
    (not (string=? s "false")))

  (: parse-owner (Sxml -> Owner))
  (define (parse-owner sxml)
    (define sx-id (select-single-node-text "/s3:Owner/s3:ID" nss))
    (define sx-name (select-single-node-text "/s3:Owner/s3:DisplayName" nss))
    (let ((id (sx-id sxml))
	(name (sx-name sxml)))
      (Owner id name)))

  (: parse-object (Sxml -> Object))
  (define (parse-object sxml)
    (define sx-key (select-single-node-text "/s3:Key" nss))
    (define sx-last-modified (select-single-node-text "/s3:LastModified" nss))
    (define sx-etag (select-single-node-text "/s3:ETag" nss))
    (define sx-size (select-single-node-text "/s3:Size" nss))
    (define sx-storage (select-single-node-text "/s3:storage" nss))
    (let ((key (sx-key sxml))
	(last-modified (sx-last-modified sxml))
	(etag (sx-etag sxml))
	(size (s->i (sx-size sxml)))
	(storage (sx-storage sxml))
	(owner (parse-owner sxml)))
      (Object key last-modified storage etag (assert size) owner)))
    
  (: parse-objects (Sxml -> (Listof Object)))
  (define (parse-objects sxml)
    (define sx-objects (sxpath "/s3:Contents" nss))
    (let ((objs (sx-objects sxml)))
      (if (andmap list? objs)
	 (map parse-object objs)
	 '())))

  (: parse-response (Sxml -> Objects))
  (define (parse-response sxml)
    (define sx-name (select-single-node-text "/s3:Name" nss))
    (define sx-prefix (select-single-node-text "/s3:Prefix" nss))
    (define sx-marker (select-single-node-text "/s3:Marker" nss))
    (define sx-max-keys (select-single-node-text "/s3:MaxKeys" nss))
    (define sx-is-truncated (select-single-node-text "/s3:IsTruncated" nss))
    (let ((name (sx-name sxml))
	(prefix (sx-prefix sxml))
	(marker (sx-marker sxml))
	(max-keys (s->i (sx-max-keys sxml)))
	(is-truncated (s->b (sx-is-truncated sxml)))
	(objs (parse-objects sxml)))
      (Objects name prefix marker (assert max-keys) is-truncated objs)))

  (define sx-result (sxpath "s3:ListBucketResult" nss))
  
  (let ((url (make-uri "http" #f s3-host #f 
		     (string-append "/" bucket)
		     (parms->query `(("prefix" . ,prefix)
				     ("marker" . ,marker)
				     ("max" . ,(number->string max))))
		     #f)))
    (let ((resp (s3-get-invoke (assert url))))
      (if resp
	 (parse-response (sx-result resp))
	 (error "S3 call failed")))))

;;  (s3-response-from-port (s3-get (make-list-url s3-resource) http-headers))))

;; (define (create-bucket credentials bucket)
;;   (let* ((datetime (rfc2822-date))
;;        (bucket-resource (make-s3-resource bucket (make-s3-key '())))
;;        (http-headers (list (date-header datetime) 
;; 			   (authorization-header credentials (aws-s3-auth-str "PUT" "" "" 
;; 									      datetime '() 
;; 									      (s3-resource->string bucket-resource))))))
;;     (s3-response-from-port (s3-put (make-bucket-url bucket) #"" http-headers))))

;; (define (delete-bucket credentials bucket)
;;   (let* ((datetime (rfc2822-date))
;;        (bucket-resource (make-s3-resource bucket (make-s3-key '())))
;;        (http-headers (list (date-header datetime) 
;; 			   (authorization-header credentials (aws-s3-auth-str "DELETE" "" "" datetime '() 
;; 									      (s3-resource->string bucket-resource))))))
;;     (s3-response-from-port (s3-delete (make-bucket-url bucket) http-headers))))


;; (define (make-object-url resource)
;;   (let ((url (make-base-url)))
;;     (set-url-path! url resource)
;;     url))

;; (define (put-file-object credentials in-file-path s3-resource)
;;   (when (file-exists? in-file-path)
;;     (let* ((size (file-size in-file-path))
;; 	 (ip (open-input-file in-file-path))
;; 	 (buff (read-bytes size ip))
;; 	 (close-input-port ip))
;;       (put-object credentials buff s3-resource))))

;; (define (put-object credentials bytes s3-resource)
;;   (let* ((size (bytes-length bytes))
;;        (hash64 (bytes->string/utf-8 (let ((enc (base64-encode (md5 bytes #f)))) 
;; 				      (subbytes enc 0 (- (bytes-length enc) 2))))) ;; base64-encode adds a bogus \r\n 
;;        (mime "binary/octet-stream")
;;        (datetime (rfc2822-date))
;;        (url (make-object-url s3-resource))
;;        (http-headers (list (date-header datetime)
;; 			   (content-type mime)                         
;; 			   (content-md5 hash64)
;; 			   (authorization-header credentials 
;; 						 (aws-s3-auth-str "PUT" hash64 mime datetime '() 
;; 								  (s3-resource->string s3-resource))))))   
;;     (s3-response-from-port (s3-put url bytes http-headers))))

;; (define (get-object credentials s3-resource)
;;   (let* ((datetime (rfc2822-date))
;;        (http-headers (list (date-header datetime)
;; 			   (authorization-header credentials 
;; 						 (aws-s3-auth-str "GET" "" "" datetime '() 
;; 								  (s3-resource->string s3-resource))))))
;;     (s3-response-from-port (s3-get (make-object-url s3-resource) http-headers))))

;; (define (head-object credentials s3-resource)
;;   (let* ((datetime (rfc2822-date))           
;;        (http-headers (list (date-header datetime)
;; 			   (authorization-header credentials 
;; 						 (aws-s3-auth-str "HEAD" "" "" datetime '() 
;; 								  (s3-resource->string s3-resource))))))
;;     (s3-response-from-port (s3-head (make-object-url s3-resource) http-headers))))

;; (define (delete-object credentials s3-resource)
;;   (let* ((datetime (rfc2822-date))
;;        (http-headers (list (date-header datetime) 
;; 			   (authorization-header credentials 
;; 						 (aws-s3-auth-str "DELETE" "" "" datetime '() 
;; 								  (s3-resource->string s3-resource))))))
;;     (s3-response-from-port (s3-delete (make-object-url s3-resource) http-headers))))
