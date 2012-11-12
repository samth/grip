#lang typed/racket/base

;; copied from racket/private/streams and <ditto>/ports
;; Racket is licensed under LGPL.

(provide:
 [if-stream-in  (Symbol (Option Input-Port)  -> (Option Input-Port))]
 [if-stream-out (Symbol (Option Output-Port) -> (Option Output-Port))]
 [pump-ports (Event Output-Port
                    (Option Input-Port)
                    (Option Input-Port)
                    Input-Port (Option Output-Port) Output-Port
                    -> (Values (U Thread (Option Output-Port))
                               (U Thread (Option Input-Port))
                               (U Thread (Option Input-Port))))])

 
;; Differentiate between a file stream port vs a generic port.

(define-type Port-Special-Value ((Option Positive-Integer) 
                                 (Option Natural) 
                                 (Option Positive-Integer) 
                                 (Option Natural) -> Any))

(: copy-port (Input-Port Output-Port -> Void))
(define (copy-port src dest)
  (let ([s (make-bytes 4096)])        
    (let loop ()
      (let ([c (read-bytes-avail! s src)])
        (cond
          [(number? c)
           (let loop ([start 0])
             (unless (= start c)
               (let ([c2 (write-bytes-avail s dest start c)])
                 (loop (+ start c2)))))              
           (loop)]
          [(procedure? c)
           (let ([v (let-values: ([([l   : (Option Exact-Positive-Integer)]
                                    [col : (Option Natural)]
                                    [p   : (Option Exact-Positive-Integer)]) (port-next-location src)])                      
                      ((cast c Port-Special-Value) (cast (object-name src) (Option Positive-Integer)) l (cast col Exact-Positive-Integer) p))])
             (write-special v dest))
           (loop)]
          [else
           ;; Must be EOF
           (void)])))))

(: if-stream-out (Symbol (Option Output-Port) -> (Option Output-Port)))
(define (if-stream-out who p)
  (cond [(or (not p) (and (output-port? p) (file-stream-port? p))) p]
        [(output-port? p) #f]
        [else (raise-type-error who "output port or #f" p)]))

(: if-stream-in (Symbol (Option Input-Port) -> (Option Input-Port)))
(define (if-stream-in who p)
  (cond [(or (not p) (and (input-port? p) (file-stream-port? p))) p]
        [(input-port? p) #f]
        [else (raise-type-error who "input port or #f" p)]))

(: streamify-in ((Option Input-Port) (Option Output-Port) (Boolean -> Void) -> (U Thread (Option Output-Port))))
(define (streamify-in cin in ready-for-break)
  (if (and in cin (not (file-stream-port? cin)))
      (thread (lambda ()
                ((inst dynamic-wind Void)
                 void
                 (lambda ()
                   (with-handlers ([exn:break? void])
                     (ready-for-break #t)
                     (copy-port cin in)
                     (ready-for-break #f)))
                 (λ () 
                   (close-output-port in)))
                (ready-for-break #t)))
      in))

(: streamify-out ((Option Output-Port) (Option Input-Port) ->  (U Thread (Option Input-Port))))
(define (streamify-out cout out)
  (if (and out cout 
           (not (eq? cout 'stdout))
           (not (file-stream-port? cout)))
      (thread (lambda ()
                (dynamic-wind
                 void
                 (lambda () (copy-port out cout))
                 (lambda () (close-input-port out)))))
      out))

(: pump-ports (Event Output-Port
                     (Option Input-Port)
                     (Option Input-Port)
                     Input-Port (Option Output-Port) Output-Port
                     -> (Values (U Thread (Option Output-Port))
                                (U Thread (Option Input-Port))
                                (U Thread (Option Input-Port)))))
(define (pump-ports evt pin pout perr in out err)
  (define: who : Symbol 'pump-ports)
  (define: it-ready : Semaphore (make-semaphore))
  (define: inpump : (U Thread (Option Output-Port))
    (streamify-in in 
                  (if-stream-out who pin)
                  (λ: ((ok? : Boolean))
                    (if ok?
                        (semaphore-post it-ready)
                        (semaphore-wait it-ready)))))
  (define: outpump : (U Thread (Option Input-Port)) 
    (streamify-out out (if-stream-in who pout)))
  (define: errpump : (U Thread (Option Input-Port)) 
    (streamify-out err (if-stream-in who perr)))
  (when (thread? inpump)
    ;; Wait for place to end, then stop copying input:
    (thread (λ ()
              (sync evt (cast inpump Event)) ;; this is avoidable, if I only knew how.
              (semaphore-wait it-ready)
              (break-thread inpump))))
  
  (values inpump outpump errpump))