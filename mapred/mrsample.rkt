#lang racket

(require racket/place/distributed
         racket/place/distributed/map-reduce)

(define-syntax define/provide
  (syntax-rules ()
    [(_ (name x ...) body ...)
     (begin (provide name)
            (define (name x ...) body ...))]
    [(_ name val)
     (begin (provide name)
            (define name val))]))

(define/provide (mapper kvs)
  (for/first ([kv kvs])
    (match kv
      [(cons k v)
       (with-input-from-file
         v
         (lambda ()
           (let loop ([result null])
             (define l (read-line))
             (if (eof-object? l)
                 result
                 (loop (cons (cons l 1) result))))))])))

(define/provide (reducer kvs)
  (for/list ([kv kvs])
    (match kv
      [(cons k v)
       (cons k (list (for/fold ([sum 0]) ([x v]) (+ sum x))))])))

(define/provide (outputer kvs)
  (displayln 
  (for/fold ([sum 0]) ([kv kvs])
;    (printf "~a - ~a\n" (car kv) (cadr kv))
    (+ sum (cadr kv)))))


(module+ main
  (define tasks
    (for/list ([i 8]) (cons i (string-append "/home/tewk/phd/benchmarks/hadoop-wc/" "w" (number->string i)))))

  (define config
    (list (list "localhost" 6431)
          (list "localhost" 6432)
;          (list "localhost" 6433)
;          (list "localhost" 6434)
          ))

  (define workers (make-map-reduce-workers config))

  (map-reduce workers config tasks
              (list (quote-module-path "..") 'mapper)
              (list (quote-module-path "..") 'reducer)
              #:combiner (list (quote-module-path "..") 'reducer)
              #:outputer (list (quote-module-path "..") 'outputer)))

