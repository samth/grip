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
 list-bucket-objects
 put-object delete-object
 put-file-object)

(require 
 racket/pretty
 (only-in (planet rpr/prelude:1/type/date)
	  current-date-string-rfc-2822)
 (only-in (planet rpr/httpclient:1/uri)
	  make-uri Uri Uri-path uri->string)
 (only-in (planet rpr/httpclient:1/http/heading)
	  DATE HOST)
 (only-in (planet rpr/httpclient:1/http/header)
	  Header
	  header->string
	  make-header
	  date-header
	  content-length
	  content-type
	  content-md5)
 (only-in (planet rpr/httpclient:1/http/http11)
	  ResponseHeader Result HTTPPayload
	  HTTPConnection-in HTTPConnection-header
	  http-invoke http-close-connection make-client-error-response)
 (only-in (planet rpr/httpclient:1/uri/url/param)
	  Params
	  params->query)
 (only-in (planet rpr/crypto:1/base64)
	  base64-encode)
 (only-in (planet rpr/crypto:1/hash/md5)
	  md5-bytes)
 (only-in (planet rpr/format:1/xml/sxml)
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

;; FIXME Use opt-map to create the parameter query string

(: list-bucket-objects (String String String String Integer -> Objects))
(define (list-bucket-objects bucket prefix delimiter marker max)

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
      (let* ((length (assert (file-size in-file-path) index?))
	     (ip (open-input-file in-file-path))
	     (mime "binary/octet-stream")
	     (payload (HTTPPayload mime #f length ip)))
	(with-handlers [(exn:fail? (lambda (ex)
				     (close-input-port ip)
				     (make-empty-error-response 500 (exn-message ex))))]
	  (s3-invoke 'PUT bucket path #f '() payload)))
      (make-empty-error-response 404 (string-append "File " 
						    in-file-path 
						    " does not exist to PUT"))))

(: put-object (Bytes String String -> S3Response))
(define (put-object bytes bucket path)
  (let* ((length (bytes-length bytes))
	 (md5 (base64-encode (md5-bytes bytes)))
	 (mime "binary/octet-stream")
	 (payload (HTTPPayload mime md5 length (open-input-bytes bytes))))
    (s3-invoke 'PUT bucket path #f '() payload)))

(: delete-object (String String -> S3Response))
(define (delete-object bucket path)
  (s3-invoke 'DELETE bucket path #f '() #f))