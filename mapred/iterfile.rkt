#lang typed/racket/base

(provide 
 OK
 (struct-out IOResult)
 (struct-out IOSuccess)
 (struct-out IOFailure)
 iter-file
 iter-block
 iter-file-record
 iter-text-file
 text-list-iteratee
 TextFileIteratee)

(require 
 racket/match
 (only-in io/iteratee/iteratee
          Iteratee Stream Continue Done)
 (only-in "types.rkt"
          Text Record Record-key Record-value
          Block Block-loc)
 (only-in "rdd/rdd.rkt"
         generate-rdd-block-filename))

#| Iteratee for File output. |#

#| 
An often commented on failing of Iteratees, especially in the Haskell world,
is that an Iteratee is unable to handle exceptions.  Specifically, that is an impossible to 
write IterFile which does not leak the open file handle on exceptions. 

Certainly, we could do that here.  However, currently I believe it is better to simply punt the 
Resource leak issue beyond the Iteratee's purview by using resource regions/barriers. 

In Racket this would imply the use of Guardians etc. to create this barrier.  Therefore, no attempt
on for example to wrestle with IO exceptions is in any of the file iteratees below.

Of course this puts an obligation on the library user to properly establish the resource barrier.
|#

(define-type TextFileIteratee (Iteratee String IOResult))

(struct: IOResult ([msg : String]) #:transparent)
(struct: IOSuccess IOResult ())
(struct: IOFailure IOResult ())

(define OK (IOSuccess "OK"))

(: iter-file-blocks (All (D) (D Output-Port -> Void) (U 'text 'binary) -> (Iteratee D (Listof (Block D)))))
(define (iter-file-blocks writer mode)
  
  (define size-limit (* 1024 1024 64)) ;; 64meg
  
  (: rollover? (Output-Port -> Boolean))
  (define (rollover? pout)
    (> (file-position pout) size-limit))
  
  (: port-rollover (Output-Port (Listof Block) -> (Values Output-Port (Listof Block))))
  (define (port-rollover pout blocks)
    (let ((sz (file-position pout)))          
      (close-output-port pout)
      (let ((path (generate-rdd-block-filename)))
        (if (pair? blocks)
            (let ((block (car blocks)))
              (let ((blocks (cons (Block (Block-loc block) 0 sz) blocks))
                    (pout (open-output-file path #:mode mode)))
                (values pout (cons (Block path 0 0) blocks))))
            (values pout (list ((inst Block D) path 0 0))))))) ;; can't happen
                  
  (: step (Output-Port (Listof (Block D)) -> ((Stream D) -> (Iteratee D (Listof (Block D))))))
  (define (step pout blocks)
    (λ: ((record : (Stream D)))
      (cond 
        ([eq? record 'Nothing]
         (Continue (step pout blocks)))
        ([eq? record 'EOS]
         (begin
           (close-output-port pout)
           (Done 'EOS blocks)))
        (else
         (if (rollover? pout)
             (let-values (((pout blocks) (port-rollover pout blocks)))
               (writer record pout)
               (Continue (step pout blocks)))
             (begin
               (writer record pout)
               (Continue (step pout blocks))))))))
  
  (let ((path (generate-rdd-block-filename)))
    (Continue (step (open-output-file path #:mode mode)
                    (list ((inst Block D) path 0 0))))))

(: iter-block (All (D) 
                   (case-> (Path (D Output-Port -> Void) [#:mode (U 'text 'binary)] -> (Iteratee D (Listof (Block D))))
                           (Path (D Output-Port -> Void) [#:mode (U 'text 'binary)] -> (Iteratee D (Listof (Block D)))))))
(define (iter-block path writer #:mode [mode 'text])
  
  (: pout Output-Port)
  (define pout (open-output-file path #:mode mode #:exists 'append))
  
  (: step ((Listof (Block D)) -> ((Stream D) -> (Iteratee D (Listof (Block D))))))
  (define (step blocks)
    (λ: ((record : (Stream D)))
      (cond
        ([eq? record 'Nothing]
         (Continue (step blocks)))
        ([eq? record 'EOS]         
         (let ((blks (match blocks
                       [(list-rest h t) 
                        (cons (Block (Block-loc h) 0 (file-position pout)) t)]
                       [else blocks])))
           (close-output-port pout)
           (Done 'EOS blks)))
        (else 
         (writer record pout)
         (Continue (step blocks))))))
  
  (Continue (step (list (Block path 0 0)))))

(: iter-file (All (D) Path (D Output-Port -> Void) [#:mode (U 'text 'binary)] -> (Iteratee D IOResult)))
(define (iter-file path writer #:mode [mode 'text])
  
  (: pout Output-Port)
  (define pout (open-output-file path #:mode mode))
  
  (: step ((Stream D) -> (Iteratee D IOResult)))
  (define step
    (λ: ((record : (Stream D)))
      (cond
        ([eq? record 'Nothing]
         (Continue step))
        ([eq? record 'EOS]
         (begin
           (close-output-port pout)
           (Done 'EOS OK)))
        (else 
         (writer record pout)
         (Continue step)))))
  
  (Continue step))                     

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

(: iter-text-file (case->
                   (Path Boolean -> TextFileIteratee)
                   (Path -> TextFileIteratee)))
(define (iter-text-file path [append? #f])
  
  (: pout Output-Port)
  (define pout
    (if append?        
        (open-output-file path #:mode 'text #:exists 'append)
        (open-output-file path #:mode 'text)))
  
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

(: text-list-iteratee (-> (Iteratee Text (Listof Text))))
(define (text-list-iteratee)
  
  (: step ((Listof Text) -> ((Stream Text) -> (Iteratee Text (Listof Text)))))
  (define (step text)
    (λ: ((txt : (Stream Text)))
      (cond
        [(eq? txt 'Nothing)
         (Continue (step text))]
        ([eq? txt 'EOS]
         (Done 'EOS text))
        (else (Continue (step (cons txt text)))))))
  
  (Continue (step #{'() : (Listof Text)})))

