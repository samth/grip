(module s3types-test mzscheme
  
  (require "s3types.scm"
           (planet "text-ui.ss" ("schematics" "schemeunit.plt" 2 8))
           (planet "test.ss" ("schematics" "schemeunit.plt" 2 8)))
  
  (define bucket-test
    (test-suite
     "S3 Bucket Type Test"
     (test-true "Ctor" 
                (s3-bucket? (make-s3-bucket "bravais")))
     (test-equal? "to string" 
                  (s3-bucket->string (make-s3-bucket "bravais")) "bravais")))
  
  (define key-test
    (test-suite
     "Key Type Test"
     (let ((key (make-s3-key '("a" "b" "c" "d.txt"))))
       (check-true (s3-key? key) "is type")
       (test-equal? "->string" "a/b/c/d.txt" (s3-key->string key))
       (test-equal? "empty prefix" (s3-key->string (make-s3-key '())) "")                    
       (test-equal? "prefix" 
                    (s3-key->string (make-s3-key '("a" "b" "c")))
                    "a/b/c"))
     (test-equal? "prefix with trailing /" 
                  (s3-key->string (make-s3-key '("a" "b" "c" "")))
                  "a/b/c/")))
  
  (define resource-test
    (test-suite 
     "S3 Resource Type Test"
     (let* ((bucket (make-s3-bucket "bravais"))
            (key    (make-s3-key    (list "a" "b" "c" "d.txt")))
            (resource (make-s3-resource bucket key)))
       (test-true "Ctor"
                  (s3-resource? resource))
       (test-equal? "->string"
                    (s3-resource->string resource)
                    "/bravais/a/b/c/d.txt")
       (test-case "segment encoding"
                  (check-equal? (let ((key (make-s3-key (list "a/b" "c" "d.txt"))))
                                  (s3-resource->string (make-s3-resource bucket key)))
                                "/bravais/a/b/c/d.txt")))
     (test-equal? "no bucket"
                  (s3-resource->string (make-s3-resource (make-s3-bucket "") (make-s3-key '())))
                  "/")
     (test-equal? "just bucket"
                  (s3-resource->string (make-s3-resource (make-s3-bucket "bravais") (make-s3-key '())))
                  "/bravais")
     (test-equal? "abs bucket"
                  (s3-resource->string (make-s3-resource (make-s3-bucket "bravais") (make-s3-key '(""))))
                  "/bravais/")))
  
  
  (test/text-ui 
   (test-suite "Test s3 types" 
               key-test
               bucket-test
               resource-test))
  )