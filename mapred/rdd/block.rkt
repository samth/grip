#lang typed/racket/base

(provide 
 enum/block)

(require
 racket/match
 (only-in iteratee/iteratee
          Enumerator Iteratee
          Done Continue)
 (only-in "types.rkt"  
          Block Block-loc
          Location))

(: enum/block (All (D A) Block (Input-Port -> (U EOF D))-> (Enumerator D A)))
(define (enum/block block reader)
  (define inp (open-input-file (Block-loc block)))  
  (λ: ((iter : (Iteratee D A)))
    (let loop ((iter iter))
      (match iter
        [(Done _ _) iter]
        [(Continue step)
         (let ((data (reader inp)))            
           (if (eof-object? data)
               (begin
                 (close-input-port inp) ;; FIXME RPR - IOExceptions
                 iter)
               (loop (step data))))]))))