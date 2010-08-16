#lang racket/base
 
 (provide)
 
 (require)

;;  (define supported-methods
;;    '())

;;  (define known-methods
;;    '(GET HEAD PUT DELETE OPTIONS TRACE))

;;  (define known-method?
;;    (lambda (request)
;;      (member (request-method request) known-methods)))

;;  (define supported-method?
;;    (lambda (method)
;;      (member method supported-methods)))
 
;; ;;; Condition Checks
 
;;  ;; If server is available for handling requests.
;;  (define server-available?
;;    (lambda ()
;;      #t))
 
;;  ;; Check if request is malformed
;;  (define malformed?
;;    (lambda (request)
;;      #f))

;; (define uri-too-long?
;;   (lambda (request)
;;     #f))

;; (define unauthorized?
;;   (lambda (request)
;;     #f))

;; (define forbidden?
;;   (lambda (request)
;;     #f)

;; (define method-options?
;;   (lambda (request)
;;     (eq? (request-method request) 'OPTIONS)))
  
;; ;;; Responses
 
;;  (define-syntax code-response
;;    (syntax-rules ()
;;      ((_ response code)
;;       (code-response response code ""))
;;      ((_ response code msg)
;;       (response-code-set! response code)
;;       (response-msg-set!  response msg )
;;       response)))

;; (define process-supported-method
;;   (lambda (request response)
;;     (if (resource-exists? request)
;;         (if (if-match-exists? request)
;;             (if (not (if-match*-or-etag? request))
;;                 (response-code response PRECONDITION-FAILED-412)
;;                 (if ;; -- START HERE UNMODIFIED EXISTS --
                
                

;;  ;; Process an HTTP request
;;  ;; request * response -> response 
;;  (define process
;;    (lambda (request response)
;;      (if (server-available?)
;;          (code-response response SERVICE-UNAVAILABLE-501)
;;          (if (malformed? request)
;;              (code-response response BAD-REQUEST-400)
;;              (if (uri-too-long? request)
;;                  (code-response response REQUEST-URI-TOO-LONG-414)
;;                  (if (unauthorized? request)
;;                      (code-response response UNAUTHORIZED-401)
;;                      (if (forbidden? request)
;;                          (code-response response FORBIDDEN-403)
;;                          (if (method-options? request)
;;                              (code-response response OK-200)
;;                              (if (supported-method? request)
;;                                  (process-supported-method request response)
;;                                  (if (known-method? request)
;;                                      (code-response response METHOD-NOT-ALLOWED-405)
;;                                      (code-response response NOT-IMPLEMENTED-501)))))))))))
;;  )
                                      
              
          
          
          
;;      ))
     
 
