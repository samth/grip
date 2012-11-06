#lang typed/racket/base

(provide:
 [test-db-scratch TxDB]
 [test-db TxDB]
 [test-db-p TxDB]
 [test-db-b TxDB]
 [test-db-m TxDB]
 [test-single TxDB])

(require "fptypes.rkt")
         
(: test-db TxDB)
(define test-db
  (list (Tx '(#\f #\a #\c #\d #\g #\i #\m #\p) 1)
        (Tx '(#\a #\b #\c #\f #\l #\m #\o) 1)
        (Tx '(#\b #\f #\h #\j #\o) 1)
        (Tx '(#\b #\c #\k #\s #\p) 1)
        (Tx '(#\a #\f #\c #\e #\l #\p #\m #\n) 1)))

(: test-db-p TxDB)
(define test-db-p
  (list (Tx '(#\f #\c #\a #\m) 2)
        (Tx '(#\c #\b) 1)))

(: test-db-m TxDB)
(define test-db-m
  (list (Tx '(#\f #\c #\a) 2)
        (Tx '(#\f #\c #\a #\b) 1)))
        
(define test-db-b
  (list (Tx '(#\f #\c #\a) 1)
        (Tx '(#\f) 1)
        (Tx '(#\c) 1)))

(define test-single
  (list (Tx '(#\a #\b #\c) 3)))

;; FAILING WHEN TWO ROOT CHAINS
(define test-db-scratch  
  (list 
   (Tx '(#\a #\b) 1)
   (Tx '(#\a #\c) 1)
   (Tx '(#\a #\b #\c) 1)
   (Tx '(#\a #\b #\c) 1)
   (Tx '(#\b #\c) 1)
   (Tx '(#\b) 1)))
   
       
  
  
;  (list 
;   (Tx '(#\a) 1)
;   (Tx '(#\a #\b) 1)
;   (Tx '(#\a #\b #\c) 1)))

;  (list
;   (Tx '(#\a #\b #\c) 1)
;   (Tx '(#\a #\b #\c #\d) 1)
;   (Tx '(#\a #\b #\c #\d #\e) 1)))
  
;  (list
;   (Tx '(#\a #\b) 1)
;   (Tx '(#\b #\c #\d) 1)
;   (Tx '(#\a #\c #\d #\e) 1)
;   (Tx '(#\a #\d #\e) 1)
;   (Tx '(#\a #\b #\c) 1)
;   (Tx '(#\a #\b #\c #\d) 1)
;   (Tx '(#\a) 1)
;   (Tx '(#\a #\b #\c) 1)
;   (Tx '(#\a #\b #\d) 1)
;   (Tx '(#\b #\c #\e) 1)))
;      
;  (list 
;   (Tx '(#\a #\b #\c #\d) 1)
;   (Tx '(#\a #\b) 1)
;   (Tx '(#\a) 1)
;   (Tx '(#\a #\b #\c) 1)
;;   (Tx '(#\a #\b #\c) 1)
;;   (Tx '(#\a #\e #\f) 1)
;   (Tx '(#\b #\f) 1)
;;   (Tx '(#\d #\f) 1)))
;   
;   ))
   
         