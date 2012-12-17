#lang typed/racket/base

(provide
 DEFAULT-BLOCK-SIZE
 rdd-materialization-directory)

;; The maximum length in bytes a of a data record.
(define MAX-RECORD-LENGTH 2048)

(define DEFAULT-BLOCK-SIZE (* 64 1024 1024))

;; Location of temporary working directory.
(define rdd-materialization-directory (string->path "/tmp/rktrdd"))

