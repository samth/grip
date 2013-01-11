#| Configuration routines |#

#lang typed/racket/base

;- base
;  - jobid
;    - log
;    - mp
;      - in
;      - out

(provide 
 init-configuration
 Configuration
 JobConfig
 InstallConfig
 InternalsConfig
 job-id
 job-s3-bucket)

(require 
 (only-in "prelude.rkt"
          dynamic-require/cast)
 (only-in "types.rkt"
          DynFn))

(struct: JobConfig ([id : String]
                    [name : String]
                    [s3-bucket : String]) #:transparent )

(struct: InstallConfig ([base : Path]) #:transparent)

(struct: InternalsConfig ([block-size : Natural]) #:transparent)

(struct: Configuration ([job : JobConfig]
                        [install : InstallConfig]
                        [internals : InternalsConfig]) #:transparent)

(define: CONFIGURATION : (Option Configuration) #f)

;; NOT THREAD SAFE!!!  Could mutex, not critical now.
(: init-configuration (Module-Path Symbol -> Void))
(define (init-configuration module-path config-fn)
  (unless CONFIGURATION
    (let ((dynfn (DynFn module-path config-fn)))
      (set! CONFIGURATION ((dynamic-require/cast dynfn (-> Configuration)))))))

(: configuration (-> Configuration))
(define (configuration)
  (if CONFIGURATION
      (cast CONFIGURATION Configuration)
      (error "MR Job Configuration was not performed")))

(: job-id (-> String))
(define (job-id)
  (JobConfig-id (Configuration-job (configuration))))

(: job-s3-bucket (-> String))
(define (job-s3-bucket)
  (JobConfig-s3-bucket (Configuration-job (configuration))))

(: log-directory (-> Path))
(define (log-directory)
  (build-path (InstallConfig-base (Configuration-install (configuration)))
              "log"))

(: mp-directory (-> Path))
(define (mp-directory)
  (build-path (InstallConfig-base (Configuration-install (configuration)))
              (JobConfig-id (Configuration-job (configuration)))
              "mp"))

(: working-directory-mp-in (-> Path))
(define (working-directory-mp-in)
  (build-path (mp-directory) "in"))

(: working-directory-mp-out (-> Path))
(define (working-directory-mp-out)
  (build-path (mp-directory) "out"))

