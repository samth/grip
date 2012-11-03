#lang typed/racket/base

(require 
 racket/pretty
 "iteratee.rkt"
 "iteratees.rkt"
 "enumerators.rkt"
 "enumeratees.rkt"
 "iterfile.rkt")
 
(: drop1keep1 (All (D) (-> (Iteratee D (Option D)))))
(define (drop1keep1)
  (iseq 
   ((inst drop D) 1)
   (λ (void) ((inst head D)))))

;; Drop first two, then return head element (int)
(: test-drop-ints (-> (Option Integer)))
(define (test-drop-ints)
  (icomplete (((inst enumerator/list Integer (Option Integer)) '(1 2 3 4))
              (iseq ((inst drop Integer) 2)                                               
                    (λ: ((a : Void)) ((inst head Integer)))))))

;; Drop the first 4 lines in the file
;; Then count the remaining lines.
(: test-count-file (-> Integer)) 
(define (test-count-file)
  (define infile (string->path "/etc/passwd"))
  (call-with-input-file* infile
    (λ: ((inp : Input-Port))           
      (icomplete ((enumerator/text-input-port inp) 
                  (iseq ((inst drop String) 4)
                        (λ (void)
                          ((inst counter String)))))))
    #:mode 'text))

;; Count elements int two lists by sequencing the enumerators into a single counting iteratee.
(: test-eseq (-> Integer))
(define (test-eseq)
  (icomplete ((eseq (enumerator/list '(1 2 3)) (enumerator/list '(4 7))) 
              ((inst counter Integer)))))

;; Note this (commented out stuff) shows pure Enumerators don't get consumed.
(: test-multiple-consumers (-> (Option Integer)))
(define (test-multiple-consumers)
  (let: ((lenum : (Enumerator Integer (Option Integer))
                (enumerator/list '(1 2 3 4 5 6)))
         (d2 : (Iteratee Integer Void) 
             ((inst drop Integer) 2))
         (d3 : (Iteratee Integer Void) 
             ((inst drop Integer) 3))
         (hd : (Iteratee Integer (Option Integer)) 
             ((inst head Integer))))
    ;    (pretty-print 
    ;     (list (icomplete (lenum d2))
    ;           (icomplete (lenum d3))
    ;           (icomplete (lenum hd))))
    (icomplete (lenum (iseq d2 (λ (x) (iseq d3 (λ (x) hd))))))))

(: upit (-> String))
(define (upit)
  ((inst icomplete String String) 
   (((inst enumerator/list String String) '("Hello" "World"))
    (iseq (upcase) (λ: ((s : String)) (write s)(rev s))))))

;; Enumerator which is a producer of String integers values from a list.
;; Interstitial Enumeratee converting String ints to Integers.
;; Iteratee which sums a given sequence of Integers.
(: enumT (-> Integer))
(define (enumT)
  (let: ((producer  : (Enumerator String (Iteratee Integer Integer)) 
                   (enumerator/list '("1" "2" "3" "4")))
         (converter : (Enumeratee String Integer Integer) 
                    (enumeratee-transform (λ: ((elem : String)) (assert (string->number elem) exact-integer?))))
         (consumer  : (Iteratee Integer Integer) 
                   (sum)))       
    (icomplete (icomplete (producer (converter consumer))))))

(: showit-twice (-> IOResult))
(define (showit-twice)
  (let: ((enumerator   : (Enumerator Integer (Iteratee String IOResult))
                       (enumerator/list '(1 2 3 4)))
         (doubler      : (Enumeratee Integer String IOResult)
                       (enumeratee-transform (λ: ((x : Integer)) (number->string (* x x)))))
         (persist      : TextFileIteratee
                       (iter-textfile (string->path "/run/shm/ray.txt"))))          
    (icomplete (icomplete (enumerator (doubler persist))))))
      
(: showit-twice2 (-> IOResult))
(define (showit-twice2)
  (let ((enumerator (ann (enumerator/list '(1 2 3 4)) (Enumerator Integer (Iteratee String IOResult))))
        (doubler (ann (enumeratee-transform (λ: ((x : Integer)) (number->string (* x x)))) (Enumeratee Integer String IOResult)))
        (persist (iter-textfile (string->path "/run/shm/ray.txt"))))
    (icomplete (icomplete (enumerator (doubler persist))))))
      
