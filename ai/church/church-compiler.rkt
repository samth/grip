#lang racket

(provide main)

(require racket/pretty
         (only-in "compiler.rkt" compile))

;; get filename from command line
;; read source code from file
;; read source code from external-defs file
;; call compile on code + external-defs code
;; write to filename-church.sc

(define (read-source pathname)
  (call-with-input-file pathname
    (lambda (port)
      (let loop ((forms '()))
        (let ((form (read port)))
          (if (eof-object? form)
              (reverse forms)
              (loop (cons form forms))))))))

(define (write-object objects pathname pretty)
  (call-with-output-file
      pathname
    (lambda (port)
      (for-each
       (lambda (o) ((if pretty pretty-print write) o port))
       objects))))

(define (empty-string? s)
  (equal? s ""))

(define (string->bool s)
  (cond [(equal? s "T") #t]
        [(equal? s "F") #f]
        [else (error s "string->bool: can't convert string to bool")]))

(define (main in out ext pretty)
  (let ((pretty (string->bool pretty))
        (ext-source (if (empty-string? ext) '() (read-source ext))))
    (write-object (compile (read-source in) ext-source)
                  out
                  pretty)))
