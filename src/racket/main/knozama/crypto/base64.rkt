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

#lang rnr6

(library
 (rl3 crypto base64)

 (export base64-encode base64-decode
	 base64-encode-streams base64-decode-streams)
 
(import
 (rnrs base)
 (only (rnrs arithmetic fixnums)
       fx+ fx- fx=? fx>? fx<=? fx>=? fxand fxior fxzero?
       fxarithmetic-shift fxarithmetic-shift-left fxarithmetic-shift-right)
 (only (rnrs io ports-6)
       textual-port? input-port?
       open-bytevector-input-port
       eof-object? put-char put-u8 get-bytevector-n! get-string-n!)
 (only  (rnrs bytevectors)
	make-bytevector bytevector-u8-ref bytevector-u8-set!)
 (onlyn (knozama std prelude)
	fx1+))

;;  (primitives open-output-string open-input-string get-output-string
;;              open-output-bytevector get-output-bytevector))

(define base64-encode-vector
  '#(#\A #\B #\C #\D #\E #\F #\G #\H #\I #\J #\K #\L #\M #\N #\O #\P #\Q #\R #\S #\T #\U #\V #\W #\X #\Y #\Z
     #\a #\b #\c #\d #\e #\f #\g #\h #\i #\j #\k #\l #\m #\n #\o #\p #\q #\r #\s #\t #\u #\v #\w #\x #\y #\z
     #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9 #\+ #\/))

(define base64-decode-vector
  (let ((A (char->integer #\A))
      (Z (char->integer #\Z))
      (a (char->integer #\a))
      (z (char->integer #\z))
      (zero  (char->integer #\0))
      (nine  (char->integer #\9))
      (plus  (char->integer #\+))
      (slash (char->integer #\/)))
    (let ((v (make-vector (fx1+ z) #f)))
      (do ((i 0 (fx1+ i)))
	  ((fx=? 255 i) v)
	(cond
	 ((and (fx>=? i A)
	     (fx<=? i Z))
	  (vector-set! v i (fx- i A)))
	 ((and (fx>=? i a)
	     (fx<=? i z))
	  (vector-set! v i (fx+ 26 (fx- i a))))
	 ((and (fx>=? i zero)
	     (fx<=? i nine))
	  (vector-set! v i (fx+ 52 (fx- i zero))))
	 ((fx=? i plus)
	  (vector-set! v i 62))
	 ((fx=? i slash)
	  (vector-set! v i 63)))))))

;; bip: binary-input-port
(define base64-encode-streams
  (lambda (bip top)
    ;; process stream 3 bytes as 3 * 8 bits: In  = 6 bits * 4: Out, i.e. 3 bytes in gives 4 bytes out
    ;; at the end of the input stream we have 0,1 or 2 bytes left.  For 1 or 2 padd out with nulls to 3 bytes,
    ;; and add a '=' for each null used.
    (let ([buf3 (make-bytevector 3)]
	[bput (lambda (n)
		(put-char top (vector-ref base64-encode-vector n)))]
	[paddch #\=])
      (let ([next3 (lambda ()
		   (get-bytevector-n! bip buf3 0 3))]
	  [encode (lambda (n)
		    (let ([a (bytevector-u8-ref buf3 0)]
			[b (bytevector-u8-ref buf3 1)]
			[c (bytevector-u8-ref buf3 2)])
		      (bput (fxarithmetic-shift a -2))
		      (bput (fx+ (fxand #x3f (fxarithmetic-shift a 4))
				 (fxarithmetic-shift b -4)))
		      (when (fx>? n 1)
			(bput (fx+ (fxand #x3f (fxarithmetic-shift b 2))
				   (fxarithmetic-shift c -6))))
		      (when (fx>? n 2)
			(bput (fxand #x3f c)))))])
	(let loop ((n (next3)))                     ;; read 3 bytes off of the binary stream
	  (cond
	   [(fx=? n 3)  
	    (encode n)
	    (loop (next3))]
	   [(eof-object? n) #t]
	   [(fx=? n 2)                              ;; case 2, 1, unrolled for perfomance
	    (bytevector-u8-set! buf3 2 0)           ;; we are at eos with length % 3 != 0, so we padd 1 or 2 bytes of nulls and append '=' for each null byte.
	    (encode n)
	    (put-char top paddch)
	    #t]
	   [(fx=? n 1)
	    (bytevector-u8-set! buf3 2 0)            
	    (bytevector-u8-set! buf3 1 0)
	    (encode n)
	    (put-char top paddch)
	    (put-char top paddch)
	    #t]))))))

(define base64-encode
  (lambda (bytes)
    (let ((os (open-output-string)))
      (base64-encode-streams (open-bytevector-input-port bytes) os)
      (get-output-string os))))

(define base64-decode-streams
  (lambda (tip bop)
    (let ([buf4 (make-string 5)]  ;; need extra byte from bug in Larceny portio.sch get-string-n!
	  [decode (lambda (ch)
		    (vector-ref base64-decode-vector (char->integer ch)))])
      (let ([next4 (lambda ()
		     (get-string-n! tip buf4 0 4))])
	(let loop ((n (next4)))
	  (if (or (eof-object? n)
		  (fxzero? n))   ;; Larceny bug io portio.sch
	      bop
	      (let ((b1 (decode (string-ref buf4 0)))
		    (b2 (decode (string-ref buf4 1)))
		    (b3 (decode (string-ref buf4 2)))
		    (b4 (decode (string-ref buf4 3))))
		(put-u8 bop (fxior (fxarithmetic-shift-left  b1 2)
				   (fxarithmetic-shift-right b2 4)))
		(when b3
		  (put-u8 bop (fxior (fxarithmetic-shift-left  (fxand b2 #x0f) 4)
				     (fxarithmetic-shift-right b3 2))))
		(when b4
		  (put-u8 bop (fxior (fxarithmetic-shift-left (fxand b3 #x03) 6)
				     b4)))
		(loop (next4)))))))))

;; NOTE DOES NOT WORK AS LARCENY HAS BUG (SKIPS nth CHAR) in get-string-n!
(define base64-decode
  (lambda (str)
    (let ((bop (open-output-bytevector)))
      (base64-decode-streams (open-input-string str) bop)
      (get-output-bytevector bop))))
