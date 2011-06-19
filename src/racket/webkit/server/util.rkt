#lang racket/base

(provide
 string-split
 normalize-path)

(require
 racket/fixnum)

;; Given a list of path segments normalize them by removing ".." and "." segments
;; Note: Never allow ".." to go above the relative root.
(define normalize-path 
  (lambda (segs)
    (let loop ((segs segs) (accum '()))
      (if (null? segs)
	 (reverse accum)
	 (let ((seg (car segs)))
	   (cond
	    ((string=? seg ".")
	     (loop (cdr segs) accum))
	    ((string=? seg "..")
	     (if (and (pair? accum)
		   (not (string=? (car accum) "")))
		(loop (cdr segs) (cdr accum))
		#f))
	    (else (loop (cdr segs) (cons seg accum)))))))))

                                        ;
;;Split a string into runs for which the predicate tests true.
;;
;; (string-split "" char?) => ()
;; (string-split "abc" char?) => ("abc")
;; (string-split "a12b34cd56e" char-numeric?) => ("12" "34" "56")

(define (string-split s constituent?)
  (let ((limit (string-length s)))
    (let loop ((i 0) (words '()))
      (cond ((fx>= i limit) 
	     (reverse words))
	    ((constituent? (string-ref s i))
	     (let ((start i))
	       (let loop2 ((i (add1 i)))
		 (if (and (fx< i limit) (constituent? (string-ref s i)))
		    (loop2 (add1 i))
		    (loop (add1 i) (cons (substring s start i) words))))))
	    (else
	     (loop (add1 i) words))))))

;; Probably simplistic.  The ECMAScript spec has a more complete
;; definition of how to encode and decode urls and url components.

(define (decode-url s)
  (let loop ((l (string->list s)) (r '()))
    (cond ((null? l)
	   (list->string (reverse r)))
	  ((and (char=? (car l) #\%) 
	      (not (null? (cdr l)))
	      (char-hexdigit? (cadr l))
	      (not (null? (cddr l)))
	      (char-hexdigit? (caddr l)))
	   (loop (cdddr l) 
		 (cons (integer->char (+ (* 16 (hexdigit-value (cadr l)))
					 (hexdigit-value (caddr l))))
		       r)))
	  (else
	   (loop (cdr l) (cons (car l) r))))))

(define (char-hexdigit? c)
  (or (char-numeric? c)
     (and (char<=? #\a c) (char<=? c #\f))
     (and (char<=? #\A c) (char<=? c #\F))))

(define (hexdigit-value c)
  (if (char-numeric? c)
     (fx- (char->integer c) (char->integer #\0))
     (fx+ 10 (- (char->integer (char-upcase c)) (char->integer #\A)))))

