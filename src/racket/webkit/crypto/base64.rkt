;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Knozama's Amazon API Library
;; Copyright (C) 2007,2008,2009,2010  Raymond Paul Racine
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#lang typed/racket/base

(provide base64-encode base64-decode
	 base64-encode-streams base64-decode-streams)

(require
 racket/fixnum)

(require/typed racket/base
 (read-bytes! (Bytes Input-Port Integer Integer -> (U EOF Integer))))

(define-syntax fx1+
  (syntax-rules ()
    ((fx1+ x)
     (fx+ 1 x))))

(: base64-encode-vector (Vectorof Char))
(define base64-encode-vector
  '#(#\A #\B #\C #\D #\E #\F #\G #\H #\I #\J #\K #\L #\M #\N #\O #\P #\Q #\R #\S #\T #\U #\V #\W #\X #\Y #\Z
     #\a #\b #\c #\d #\e #\f #\g #\h #\i #\j #\k #\l #\m #\n #\o #\p #\q #\r #\s #\t #\u #\v #\w #\x #\y #\z
     #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9 #\+ #\/))

(: the-encode-char (Integer -> Char))
(define (the-encode-char idx)
  (vector-ref base64-encode-vector idx))

(: base64-decode-vector Bytes)
(define base64-decode-vector
  (let ((A (char->integer #\A))
      (Z (char->integer #\Z))
      (a (char->integer #\a))
      (z (char->integer #\z))
      (zero  (char->integer #\0))
      (nine  (char->integer #\9))
      (plus  (char->integer #\+))
      (slash (char->integer #\/)))
    (let: ((v : Bytes (make-bytes (fx1+ z) 0)))
      (do  ((i 0 (fx1+ i)))
	  ((fx= 255 i) v)
	(cond
	 ((and (fx>= i A)
	     (fx<= i Z))
	  (bytes-set! v i (- i A)))
	 ((and (fx>= i a)
	     (fx<= i z))
	  (bytes-set! v i (fx+ 26 (fx- i a))))
	 ((and (fx>= i zero)
	     (fx<= i nine))
	  (bytes-set! v i (fx+ 52 (fx- i zero))))
	 ((fx= i plus)
	  (bytes-set! v i 62))
	 ((fx= i slash)
	  (bytes-set! v i 63)))))))

(: base64-encode-streams (Input-Port Output-Port -> Boolean))
(define (base64-encode-streams bip top)
  ;; process stream 3 bytes as 3 * 8 bits: In  = 6 bits * 4: Out, i.e. 3 bytes in gives 4 bytes out
  ;; at the end of the input stream we have 0,1 or 2 bytes left.  For 1 or 2 padd out with nulls to 3 bytes,
  ;; and add a '=' for each null used.
  (let ([buf3 (make-bytes 3)]
      [bput (lambda:  ((n : Integer))
	      (write-char (the-encode-char n) top))]
      [paddch #\=])
    (let ([next3 (lambda ()
		 (read-bytes! buf3 bip 0 3))]
	[encode (lambda: ((n : Byte))
		  (let ([a (bytes-ref buf3 0)]
		      [b (bytes-ref buf3 1)]
		      [c (bytes-ref buf3 2)])
		    (bput (fxlshift a -2))
		    (bput (fx+ (fxand #x3f (fxlshift a 4))
			       (fxlshift b -4)))
		    (when (fx> n 1)
		      (bput (fx+ (fxand #x3f (fxlshift b 2))
				 (fxlshift c -6))))
		    (when (fx> n 2)
		      (bput (fxand #x3f c)))))])
      (let: loop : Boolean ((n : (U EOF Integer) (next3)))                     ;; read 3 bytes off of the binary stream
	(cond
	 [(eof-object? n) #t]
	 [(fx= n 3)  
	  (encode n)
	  (loop (next3))]
	 [(fx= n 2)                              ;; case 2, 1, unrolled for perfomance
	  (bytes-set! buf3 2 0)           ;; we are at eos with length % 3 != 0, so we padd 1 or 2 bytes of nulls and append '=' for each null byte.
	  (encode n)
	  (write-char paddch top)
	  #t]
	 [(fx= n 1)
	  (bytes-set! buf3 2 0)            
	  (bytes-set! buf3 1 0)
	  (encode n)
	  (write-char paddch top)
	  (write-char paddch top)
	  #t]
	 (else #f))))))

(: base64-encode (Bytes -> String))
(define (base64-encode bytes)
  (let ((os (open-output-string)))
    (base64-encode-streams (open-input-bytes bytes) os)
    (get-output-string os)))


(: base64-decode-streams (Input-Port Output-Port -> Output-Port))
(define base64-decode-streams
  (lambda (tip bop)
    (let ([buf4 (make-string 5)]  ;; need extra byte from bug in Larceny portio.sch get-string-n!
	[decode (lambda: ((ch : Char))
		  (bytes-ref base64-decode-vector (char->integer ch)))])
      (let ([next4 (lambda ()
		   (read-string! buf4 tip 0 4))])
	(let: loop : Output-Port ((n : (U EOF Integer) (next4)))
	  (if (or (eof-object? n)
		(zero? n))   ;; Larceny bug io portio.sch
	     bop
	     (let ((b1 (decode (string-ref buf4 0)))
		 (b2 (decode (string-ref buf4 1)))
		 (b3 (decode (string-ref buf4 2)))
		 (b4 (decode (string-ref buf4 3))))
	       (write-byte (fxior (fxlshift b1 2)
				  (fxrshift b2 4)) bop)
	       (when b3
		 (write-byte (fxior (fxlshift (fxand b2 #x0f) 4)
				    (fxrshift b3 2)) bop))
	       (when b4
		 (write-byte (fxior (fxlshift (fxand b3 #x03) 6) 
				    b4) bop))
	       (loop (next4)))))))))

;; NOTE DOES NOT WORK AS LARCENY HAS BUG (SKIPS nth CHAR) in get-string-n!
(: base64-decode (String -> Bytes))
(define base64-decode
  (lambda (str)
    (let ((bop (open-output-bytes)))
      (base64-decode-streams (open-input-string str) bop)
      (get-output-bytes bop))))
