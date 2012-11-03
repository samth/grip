#lang typed/racket/base

(provide
 rdd-materialization-directory)

;; The maximum length in bytes a of a data record.
(define MAX-RECORD-LENGTH 2048)

;; Location of temporary working directory.
(define rdd-materialization-directory (string->path "/tmp/rktrdd"))
