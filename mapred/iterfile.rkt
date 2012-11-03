#lang typed/racket/base

(provide 
 iter-file-record
 iter-textfile
 TextFileIteratee)

(require 
 (only-in iteratee/iteratee
          Iteratee Stream Continue Done)
 (only-in "types.rkt"
          Record Record-key Record-value))

#| Iteratee for File output. |#

#| 
An often commented on failing of Iteratees, especially in the Haskell world,
is that an Iteratee is unable to handle exceptions.  Specifically, that is an impossible to 
write IterFile which does not leak the open file handle on exceptions. 

Certainly, we could do that here.  However, currently I believe it is better to simply punt the 
Resource leak issue beyond the Iteratee's purview buy using resource regions/barriers. 

In Racket this would imply the use of Guardians etc. to create this barrier.  Therefore, no attempt
on for example to wrestle with IO exceptions is in any of the file iteratees below.

Of course this puts an obligation on the library user to properly establish the resource barrier.
|#

(define-type TextFileIteratee (Iteratee String IOResult))

(struct: IOResult ([msg : String]) #:transparent)
(struct: IOSuccess IOResult ())
(struct: IOFailure IOResult ())

(define OK (IOSuccess "OK"))
         
(: iter-file-record (All (K V) Path (K -> String) (V -> String) -> (Iteratee (Record K V) IOResult)))
(define (iter-file-record path key-formatter value-formatter) 
  
  (define: pout : Output-Port (open-output-file path))
 
  (: step ((Stream (Record K V)) -> (Iteratee (Record K V) IOResult)))
  (define step
    (λ: ((record : (Stream (Record K V))))
      (cond 
        ([eq? record 'Nothing] (Continue step))
        ([eq? record 'EOS]     
         (begin           
           (close-output-port pout)
           (Done 'EOS OK)))
        (else (begin
                (display (key-formatter (Record-key record)) pout))
                (displayln (value-formatter (Record-value record)) pout)
                (Continue step)))))
  
  (Continue step))

(: iter-textfile (Path -> TextFileIteratee))
(define (iter-textfile path)
  
  (define: pout : Output-Port (open-output-file path))      
  
  (: step ((Stream String) -> (Iteratee String IOResult)))
  (define step
    (λ: ((s : (Stream String)))
      (cond
        ([eq? s 'Nothing] 
         (Continue step))
        ([eq? s 'EOS]
         (close-output-port pout)
         (Done 'EOS  OK))
        (else (begin
                (displayln s pout)
                (Continue step))))))
  
  (Continue step))
  
  