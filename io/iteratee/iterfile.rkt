#lang typed/racket/base

(provide 
 iter-text-file
 iter-text-port
 TextFileIteratee
 TextPortIteratee
 OK
 (struct-out IOResult)
 (struct-out IOSuccess)
 (struct-out IOFailure))

(require 
 (only-in "iteratee.rkt"
          Iteratee Stream Continue Done))

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
(define-type TextPortIteratee (Iteratee String IOResult))

(define-type OutputFilePortIteratee (Iteratee String IOResult))

(struct: IOResult ([msg : String]) #:transparent)
(struct: IOSuccess IOResult ())
(struct: IOFailure IOResult ())

(define OK (IOSuccess "OK"))

(: iter-text-port (Output-Port -> TextPortIteratee))
(define (iter-text-port outp)
  
  (: step ((Stream String) -> (Iteratee String IOResult)))
  (define step
    (λ: ((s : (Stream String)))
	(cond
	 ([eq? s 'Nothing] 
	  (Continue step))
	 ([eq? s 'EOS]
	  (flush-output outp)
	  (Done 'EOS  OK))
	 (else (begin
		 (displayln s outp)
		 (Continue step))))))
  
  (Continue step))

(: iter-text-file (Path -> TextFileIteratee))
(define (iter-text-file path)
  
  (define: outp : Output-Port (open-output-file path))      
  
  (: step ((Stream String) -> (Iteratee String IOResult)))
  (define step
    (λ: ((s : (Stream String)))
	(cond
	 ([eq? s 'Nothing] 
	  (Continue step))
	 ([eq? s 'EOS]
	  (close-output-port outp)
	  (Done 'EOS  OK))
	 (else (begin
		 (displayln s outp)
		 (Continue step))))))
  
  (Continue step))

