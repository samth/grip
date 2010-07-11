
(import
 (rnrs base)
 (only (rnrs control)
       when)
 (only (rl3 env debug)
       debug-enable)
 (rl3 test unit-tests)
 (rl3 aws configuration))

(debug-enable #f)

(begin
  
  (when (test-runner-current)
    (test-runner-reset (test-runner-current)))
  
  (test-begin "AWS Configuration")

  (test-equal? "Lookup standard S3 host"
               (s3-configuration 'host)
               "s3.amazonaws.com")
  
  (test-end "AWS Configuration"))
