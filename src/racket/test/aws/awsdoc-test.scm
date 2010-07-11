
(require "awsauth.sls")

(define test-key "OtxrzxIsfpFjA7SwPzILwy8Bw21TLhquhboDYROV")

(aws-s3-auth-mac-encode test-key 
                        (aws-s3-auth-str "PUT" "c8fdb181845a4ca6b8fec737b3581d76" "text/html" "Thu, 17 Nov 2005 18:49:58 GMT"
                                         (list "x-amz-magic:abracadabra" "x-amz-meta-author:foo@bar.com")
                                         "/quotes/nelson"))


;(module awsdoc-test mzscheme
;  
;  (require 
;   "s3types.scm"   
;   "awsauth.scm"
;   "awscredentials.scm"
;   (planet "text-ui.ss" ("schematics" "schemeunit.plt" 2 8))
;   (planet "test.ss" ("schematics" "schemeunit.plt" 2 8)))
;  
;  (define account-props "/home/ray/awsaccount.txt") 
;  (define credentials (load-credentials account-props))  
;  (define test-key "OtxrzxIsfpFjA7SwPzILwy8Bw21TLhquhboDYROV")
;  
;  (define aws-signing-suite
;    (test-suite
;     "AWS Document Signing Examples Test"
;     (let* ((key (make-s3-key '("quotes" "nelson")))
;            (bucket (make-s3-bucket "bravaistest"))
;            (resource (make-s3-resource bucket key)))
;       
;       (check-true (s3-resource? resource) "is type")
;       
;       (test-equal? "1st Example" (aws-s3-auth-str "PUT" "c8fdb181845a4ca6b8fec737b3581d76" "text/html" "Thu, 17 Nov 2005 18:49:58 GMT"
;                                                   (list "x-amz-magic:abracadabra" "x-amz-meta-author:foo@bar.com") 
;                                                   "/quotes/nelson")
;                    "AWS 15B4D3461F177624206A:Iuyz3d3P0aTou39dzbq7RrtSFmw="))))
;  
;  
;  (test/text-ui 
;   (test-suite "S3 Document Signing Examples" 
;               aws-signing-suite))
;  

;           (test-equal? (aws-s3-rpc-str "PUT" "c8fdb181845a4ca6b8fec737b3581d76" "text/html" "Thu, 17 Nov 2005 18:49:58 GMT"
;                                        (list "x-amz-magic:abracadabra" "x-amz-meta-author:foo@bar.com")
;                                        "/quotes/nelson")                    
;
;;; AWS Sign
;(define rpc-str 
;(write rpc-str)(newline)
;
;;; using aws field to override date 
;;; I should never need this as its only if I cannot control the header date.
;(define rpc-str2 (aws-s3-rpc-str "GET" "" "" ""
;                                (list "x-amz-date:Thu, 17 Nov 2005 18:49:58 GMT" "x-amz-magic:abracadabra")
;                                "/quotes/nelson"))
;
;(define rpc-str3 (aws-s3-rpc-str "GET" "" "" "1141889120"
;                                '()
;                                "/quotes/nelson"))
;
;(write rpc-str2)(newline)
;(write rpc-str3)(newline)
;
;(aws-rpc-mac test-key rpc-str)
;(aws-rpc-mac test-key rpc-str2)
;(aws-rpc-mac test-key rpc-str3)
;
;(url->string (make-url "http" #f "s3.amazonaws.com" #f #t 
;                       (list (make-path/param  "mybucket" '()) 
;                             (make-path/param "prefix" '())) 
;                       '((x . "ray")(y . "racine")) #f))
;
;






;                    (define iop (open-lrf-file test-file))
;                    (define hdr (read-header iop))
;                    (dump hdr)
;                    (display (xml-metadata hdr iop))
;                    (close-lrf-file iop)
