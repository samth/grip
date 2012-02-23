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
 s3-list-bucket-objects)

(require 
 racket/pretty
 (only-in (planet knozama/common:1/type/date)
	  current-date-string-rfc-2822)
 (only-in (planet knozama/webkit:1/web/uri)
	  make-uri Uri Uri-path uri->string)
 (only-in (planet knozama/webkit:1/web/http/heading)
	  DATE HOST)
 (only-in (planet knozama/webkit:1/web/http/header)
	  Header
	  header->string
	  make-header
	  date-header
	  content-length
	  content-type
	  content-md5)
 (only-in (planet knozama/webkit:1/web/http/http11)
	  ResponseHeader Result
	  HTTPConnection-in HTTPConnection-header
	  http-invoke http-close-connection make-client-error-response)
 (only-in (planet knozama/webkit:1/web/uri/url/param)
	  Params
	  params->query)
 (only-in (planet knozama/webkit:1/crypto/base64)
	  base64-encode)
 (only-in (planet knozama/webkit:1/crypto/hash/md5)
	  md5-bytes)
 (only-in (planet knozama/xml:1/sxml)
	  Sxml SXPath 
	  sxpath xml->sxml select-single-node-text)
 (only-in "../configuration.rkt"
	  s3-host)
 (only-in "types.rkt" 
	  Owner Bucket Buckets Objects Object)
 (only-in "configuration.rkt"
	  nss)
 (only-in "invoke.rkt"
	  make-base-uri make-empty-error-response s3-invoke
	  S3Response S3Response-sxml
	  S3Payload))

(: aws-error (String -> Void))
(define (aws-error s)
  (display s) (newline))


;; (define (make-bucket-url bucket)
;;   (let ((url (make-base-url)))
;;     (set-url-path! url (list bucket))
;;     url))    

;; FIXME Use opt-map to create the parameter query string

;;(: list-bucket-objects (String Keyword String Keyword String Keyword Integer -> (Listof Any)))
;;(define (list-bucket-objects bucket #:prefix prefix #:marker marker #:max max)

(: s3-list-bucket-objects (String String String String Integer -> Objects))
(define (s3-list-bucket-objects bucket prefix delimiter marker max)

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
    ;;(define sx-storage (select-single-node-text "/s3:StorageClass" nss))
    (let ((key (sx-key sxml))
	  (last-modified (sx-last-modified sxml))
	  (etag (sx-etag sxml))
	  (size (s->i (sx-size sxml)))
	  ;;(storage (sx-storage sxml))
	  (owner (parse-owner sxml)))
      (Object key last-modified ;; storage 
	      etag (assert size) owner)))
  
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
  
  (let ((query `(("prefix" . ,prefix)
		 ("marker" . ,marker)
		 ("delimiter" . ,delimiter)
		 ("max" . ,(number->string max)))))
    (let ((resp (s3-invoke 'GET bucket "" query '() #f)))
      (parse-response (sx-result (S3Response-sxml resp))))))

(: put-file-object (String String String -> S3Response))
(define (put-file-object in-file-path bucket path)
  (if (file-exists? in-file-path)
      (let* ((size (file-size in-file-path))
	     (ip (open-input-file in-file-path))
	     (buff (read-bytes size ip))
	     (close-input-port ip))
	(if (not (eof-object? buff))
	    (put-object buff bucket path)
	    (make-empty-error-response 404 (string-append "File " in-file-path " is 0 byte file."))))
      (make-empty-error-response 404 (string-append "File " in-file-path " does not exist to PUT"))))

(: put-object (Bytes String String -> S3Response))
(define (put-object bytes bucket path)
  (let* ((md5 (base64-encode (md5-bytes bytes)))
	 (mime "binary/octet-stream")
	 (payload (S3Payload mime md5 (open-input-bytes bytes))))
    (s3-invoke 'PUT bucket path #f '() payload)))

      ;; (with-handlers [(exn:fail? (lambda (ex)
      ;; 				   (http-close-connection connection)
      ;; 				   (make-client-error-response 500 (exn-message ex))))]
      ;; 	(HTTPConnection-header connection)))
    
    ;; (if (has-content-length response)
    ;; 	(begin
    ;; 	  (parse-s3-error (xml->sxml (HTTPConnection-in connection) '()))
    ;; 	  (http-close-connection connection)
    ;; 	  (HTTPConnection-header connection))
    ;; 	(make-client-error-response 400 "Bad URL given in client call - not invoking server"))))
    ;;(error "Invalid URL in S3 call")))

;; (: delete-object (String String -> ResponseHeader))
;; (define (delete-object bucket path)
;;   (let* ((datetime (current-date-string-rfc-2822))
;; 	 (url (make-base-uri bucket path))
;; 	 (headers (map header->string
;; 		       (list ;; (date-header datetime)
;; 			(authorization-header (current-aws-credential)
;; 					      (aws-auth-str "DELETE" "" "" datetime '() 
;; 							    (string-append "/" bucket path)))))))
;;     (if url
;; 	(let ((connection (http-invoke 'DELETE url headers #f)))
;; 	  (with-handlers [(exn:fail? (lambda (ex)
;; 				       (http-close-connection connection)
;; 				       (make-client-error-response 500 (exn-message ex))))]
;; 	    (pretty-print (xml->sxml (HTTPConnection-in connection) '()))
;; 	    (http-close-connection connection)
;; 	    (HTTPConnection-header connection)))
;; 	(make-client-error-response 400 "Bad URL given in client call - not invoking server"))))


;; ;; FIX ME - Sometimes the response has expository xml payload e.g. for a failed delete.
;; ;; consider struct S3Response ([http : ResponseHeader][payload : Sxml]) or S3Error

;; (: test-put (-> ResponseHeader))
;; (define (test-put)
;;   (put-file-object "/home/ray/test.dat" "knozama" "/test/test.dat"))

;; (: test-delete (-> ResponseHeader))
;; (define (test-delete)
;;   (delete-object "knozama" "/test/test.dat"))


;; (: get-object (AwsCredential String String -> Void))
;; (define (get-object credentials bucket path)
;;   (let* ((datetime (current-date-string-rfc-2822))	 
;; 	 (http-headers (list (date-header datetime)
;; 			     (authorization-header credentials 
;; 						   (aws-auth-str "GET" 
;; 								 "" 
;; 								 "" 
;; 								 datetime 
;; 								 '()
;; 								 (string-append "/" bucket path))))))
;;     (let ((uri (make-base-uri bucket path)))
;;       (when uri
;; 	(pretty-print (s3-get-invoke uri))))))

;; ;; (define (head-object credentials s3-resource)
;; ;;   (let* ((datetime (rfc2822-date))           
;; ;;        (http-headers (list (date-header datetime)
;; ;; 			   (authorization-header credentials 
;; ;; 						 (aws-s3-auth-str "HEAD" "" "" datetime '() 
;; ;; 								  (s3-resource->string s3-resource))))))
;; ;;     (s3-response-from-port (s3-head (make-object-url s3-resource) http-headers))))

;; ;; (define (delete-object credentials s3-resource)
;; ;;   (let* ((datetime (rfc2822-date))
;; ;;        (http-headers (list (date-header datetime) 
;; ;; 			   (authorization-header credentials 
;; ;; 						 (aws-s3-auth-str "DELETE" "" "" datetime '() 
;; ;; 								  (s3-resource->string s3-resource))))))
;; ;;     (s3-response-from-port (s3-delete (make-object-url s3-resource) http-headers))))
