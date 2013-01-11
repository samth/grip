#lang typed/racket/base

(provide
 job-configuration
 DEFAULT-BLOCK-SIZE
 working-directory
 rdd-materialization-directory)

(require 
 (only-in "configuration.rkt"
          Configuration JobConfig InstallConfig InternalsConfig))

;; The maximum length in bytes a of a data record.
(define MAX-RECORD-LENGTH 2048)

(define DEFAULT-BLOCK-SIZE (* 64 1024 1024))

;; Location of temporary working directory.
(define rdd-materialization-directory (string->path "/tmp/rktrdd"))

(define working-directory (string->path "/run/shm/ol"))

(: job-configuration (-> Configuration))
(define (job-configuration)
  (Configuration (JobConfig "nil" "Null Job" "odbd")
                 (InstallConfig (string->path "/tmp"))
                 (InternalsConfig DEFAULT-BLOCK-SIZE)))