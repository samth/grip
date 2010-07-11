;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Bravais' Edito Princeps: EBook Tool Suite	    
;; Copyright (C) 2007  Raymond Paul Racine
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

(module s3 mzscheme   
  
  (require 
   "../bravais/bravais-properties.scm"
   "s3types.scm"
   "s3headers.scm"
   "s3-response.scm"
   "awsauth.scm"
   "awscredentials.scm"
   (lib "base64.ss" "net")
   (lib "md5.ss")
   (lib "url.ss" "net")
   (lib "date.ss")
   (lib "contract.ss"))
  
  ;;(lib "xml.ss" "xml")
  
  (provide/contract
   (list-buckets        (-> aws-credentials? s3-response?))
   (create-bucket       (-> aws-credentials? s3-bucket? s3-response?))
   (delete-bucket       (-> aws-credentials? s3-bucket? s3-response?))
   (put-object          (-> aws-credentials? bytes? s3-resource? s3-response?))
   (put-file-object     (-> aws-credentials? path? s3-resource? s3-response?))
   (head-object         (-> aws-credentials? s3-resource? s3-response?))
   (get-object          (-> aws-credentials? s3-resource? s3-response?))
   (delete-object       (-> aws-credentials? s3-resource? s3-response?))
   (list-bucket-objects (-> aws-credentials? s3-bucket? s3-key? string? number? s3-response?)))    
  
  (define s3-get    get-impure-port)
  (define s3-put    put-impure-port)
  (define s3-delete delete-impure-port)
  (define s3-head   head-impure-port)
  
  ;; Read from configuration 
  (define s3-host (bravais-s3-configuration 'host))
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; create a base http://s3.amazonaws.com url
  ;; unit -> url
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (define (make-base-url)
    (make-url "http" #f s3-host #f #t '() '() #f))
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Current time as a rfc2822 date string
  ;; "Sat, 8 Sep 2007 18:19:20 -0400"
  ;; unit -> string
  ;; Note: Is relative local TZ and _not_ GMT
  ;;       which is ok.
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (define (rfc2822-date)
    (date-display-format 'rfc2822)
    (date->string (seconds->date (current-seconds)) #t))
  
  (define (aws-error s)
    (display s) (newline))
  
  (define (authorization-header credentials auth-str)
    (string-append "Authorization: AWS " (aws-credentials-access-key credentials) 
                   ":" (aws-s3-auth-mac (aws-credentials-secret-key credentials) auth-str)))
  
  (define (make-bucket-url bucket)
    (let ((url (make-base-url)))
      (set-url-path! url (list bucket))
      url))    
  
  (define (list-buckets credentials)        
    (define (list-url)
      (let ((url (make-base-url)))
        (set-url-path! url (list (make-path/param "" '())))
        url))
    (let* ((datetime (rfc2822-date))
           (http-headers (list (date-header datetime) 
                               (authorization-header credentials (aws-s3-auth-str "GET" "" "" datetime '() "/")))))
      (s3-response-from-port (s3-get (list-url) http-headers))))
  
  (define (create-bucket credentials bucket)
    (let* ((datetime (rfc2822-date))
           (bucket-resource (make-s3-resource bucket (make-s3-key '())))
           (http-headers (list (date-header datetime) 
                               (authorization-header credentials (aws-s3-auth-str "PUT" "" "" 
                                                                                  datetime '() 
                                                                                  (s3-resource->string bucket-resource))))))
      (s3-response-from-port (s3-put (make-bucket-url bucket) #"" http-headers))))
  
  (define (delete-bucket credentials bucket)
    (let* ((datetime (rfc2822-date))
           (bucket-resource (make-s3-resource bucket (make-s3-key '())))
           (http-headers (list (date-header datetime) 
                               (authorization-header credentials (aws-s3-auth-str "DELETE" "" "" datetime '() 
                                                                                  (s3-resource->string bucket-resource))))))
      (s3-response-from-port (s3-delete (make-bucket-url bucket) http-headers))))
  
  (define (list-bucket-objects credentials bucket prefix-key marker max)
    (define (make-list-url path)
      (let ((url (make-base-url)))
        (set-url-path! url path)
        (set-url-query! url `((prefix . ,(s3-key->string prefix-key)) (marker . ,marker) (max-keys . ,(number->string max))))
        (display (url->string url))(newline)
        url))      
    (let* ((datetime (rfc2822-date))
           (s3-resource (make-s3-resource bucket (make-s3-key '())))
           (http-headers (list (date-header datetime)
                               (authorization-header credentials (aws-s3-auth-str "GET" "" "" datetime '() (s3-resource->string s3-resource))))))
      (s3-response-from-port (s3-get (make-list-url s3-resource) http-headers))))
  
  (define (make-object-url resource)
    (let ((url (make-base-url)))
      (set-url-path! url resource)
      url))
  
  (define (put-file-object credentials in-file-path s3-resource)
    (when (file-exists? in-file-path)
      (let* ((size (file-size in-file-path))
             (ip (open-input-file in-file-path))
             (buff (read-bytes size ip))
             (close-input-port ip))
        (put-object credentials buff s3-resource))))
  
  (define (put-object credentials bytes s3-resource)
    (let* ((size (bytes-length bytes))
           (hash64 (bytes->string/utf-8 (let ((enc (base64-encode (md5 bytes #f)))) 
                                          (subbytes enc 0 (- (bytes-length enc) 2))))) ;; base64-encode adds a bogus \r\n 
           (mime "binary/octet-stream")
           (datetime (rfc2822-date))
           (url (make-object-url s3-resource))
           (http-headers (list (date-header datetime)
                               (content-type mime)                         
                               (content-md5 hash64)
                               (authorization-header credentials 
                                                     (aws-s3-auth-str "PUT" hash64 mime datetime '() 
                                                                      (s3-resource->string s3-resource))))))   
      (s3-response-from-port (s3-put url bytes http-headers))))
  
  (define (get-object credentials s3-resource)
    (let* ((datetime (rfc2822-date))
           (http-headers (list (date-header datetime)
                               (authorization-header credentials 
                                                     (aws-s3-auth-str "GET" "" "" datetime '() 
                                                                      (s3-resource->string s3-resource))))))
      (s3-response-from-port (s3-get (make-object-url s3-resource) http-headers))))
  
  (define (head-object credentials s3-resource)
    (let* ((datetime (rfc2822-date))           
           (http-headers (list (date-header datetime)
                               (authorization-header credentials 
                                                     (aws-s3-auth-str "HEAD" "" "" datetime '() 
                                                                      (s3-resource->string s3-resource))))))
      (s3-response-from-port (s3-head (make-object-url s3-resource) http-headers))))
  
  (define (delete-object credentials s3-resource)
    (let* ((datetime (rfc2822-date))
           (http-headers (list (date-header datetime) 
                               (authorization-header credentials 
                                                     (aws-s3-auth-str "DELETE" "" "" datetime '() 
                                                                      (s3-resource->string s3-resource))))))
      (s3-response-from-port (s3-delete (make-object-url s3-resource) http-headers))))
  
  )
