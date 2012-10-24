#lang typed/racket/base

(provide
 enumerator/eos
 enumerator/list
 enumerator/text-input-port)

(require
 racket/match
 (only-in "iteratee.rkt"
          Done Continue Iteratee Enumerator))

(: enumerator/eos (All (D A) (Enumerator D A)))
(define (enumerator/eos iter)
  (match iter
    [(Done _ _) iter]
    [(Continue step) (step 'EOS)]))

;; Useful for testing, i.e. simulate an IO read of a string
(: enumerator/string (All (A) String -> (Enumerator String A)))
(define (enumerator/string str)
  (lambda: ((iter : (Iteratee String A)))
    (match iter
      [(Done _ _) iter]
      [(Continue k) (k str)])))

(: enumerator/list (All (D A) (Listof D) -> (Enumerator D A)))
(define (enumerator/list lst)
  (lambda: ((iter : (Iteratee D A)))
    (let: loop : (Iteratee D A) ((lst : (Listof D) lst) 
                                 (iter : (Iteratee D A) iter))
      (match (cons lst iter)
        [(cons '() i)           i]
        [(cons _ (Done _ _))    iter]
        [(cons (list-rest x xs) (Continue k)) (loop xs (k x))]))))

(: enumerator/text-input-port (All (A) (Input-Port -> (Enumerator String A))))
(define (enumerator/text-input-port inp)
  (lambda: ((iter : (Iteratee String A)))
    (let loop ((iter iter))
      (match iter
        [(Done _ _) iter]
        [(Continue step)
         (let ((line (read-line inp)))
           (if (eof-object? line)
               iter
               (loop (step line))))]))))
