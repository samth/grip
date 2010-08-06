#lang racket

(provide
 parse-x-www-form-urlencoded)

(require
 (only-in srfi/14
	  char-set-complement
	  list->char-set)
 (only-in knozama/web/uri
	  url-decode-string))


(define key-delim #\=)
(define value-delim #\&)

(define read-token
  (lambda (ip delim)
    (url-decode-string ip delim #t))) ;; decode + as space

;; parse port contents into an alist of (k . v) pairs
(define parse-x-www-form-urlencoded
  (lambda (in-port)
    (let loop ((key #f) (kvs '()))
      (if key
	 (loop #f (cons (cons key (read-token in-port value-delim)) kvs))
	 (let ((key (read-token in-port key-delim)))
	   (if (string=? key "")
	      kvs
	      (loop key kvs)))))))

