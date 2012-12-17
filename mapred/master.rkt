#lang typed/racket/base

(provide
 (struct-out MapDynFn1)
 map-reduce)

(require 
 (only-in racket/set
          list->set)
 (only-in racket/future
          processor-count)
 (only-in racket/place/distributed
          quote-module-path)
 (only-in venue/venue
          start-venue venue-event
          venue-channel-get venue-channel-put
          Venue)
 (only-in "logging.rkt"
          log-mr-info)
 (only-in "messages.rkt"
          MapTaskReqResp)
 (only-in "types.rkt"
          DynFn
          TextParser Writer Mapper Grouper
          Block Block-loc Block-sod Block-eod 
          RDDFile RDDFile-blocks)
 (only-in "tasktrack.rkt"
          make-tracker Tracker todo-count))

(struct: (A B) MapDynFn1 ([module : Module-Path]
                          [fn     : Symbol]) #:transparent)                

(struct: Topology ([venues : (Listof Venue)]))

(struct: Task ([type : (U 'Map 'Reduce)]
               [blocks : (Listof Block)]))               

(struct: MapTask ([taskid : Natural]
                  [block  : Block]
                  [dynfn  : MapDynFn1])
  #:transparent
  #:methods gen:equal+hash
  [(define (equal-proc mt1 mt2 rec?)
     (equal? (MapTask-taskid mt1) (MapTask-taskid mt2)))
   (define (hash-proc mt rec?)
     (eqv-hash-code (MapTask-taskid mt)))
   (define (hash2-proc mt rec?)
     (eqv-hash-code (* 17 (MapTask-taskid mt))))])

(struct: TaskLedger ([issued : (Listof Task)]
                     [outstanding : (Listof Task)]
                     [completed : (Listof Task)]))

(: spawn-venues (case-> (-> (Listof Venue))
                        (Natural -> (Listof Venue))))
(define (spawn-venues [cnt 0])
  (let ((cpus (processor-count)))
    (for/list: : (Listof Venue) ([n (in-range (min cnt cpus))])
      (let ((venue-name (string->symbol (string-append "MRSlave-" (number->string n)))))
        (log-mr-info "Launching slave ~s" venue-name)
        (start-venue venue-name
                     (string->path "/code/racketlib/mapred/slave.rkt")
                     'taskrunner-handler
                     #f 
                     (current-output-port)
                     (current-output-port))))))

(: make-map-request-msg (Block -> MapTaskReqResp))
(define (make-map-request-msg block)
  (MapTaskReqResp (Block-loc block) 
                  (Block-sod block) 
                  (Block-eod block)))

(: submit-map-task (Semaphore Venue MapDynFn1 Block -> Void))
(define (submit-map-task notify venue dynfn block)
  (thread (λ ()
            (log-mr-info "Submit ~s for map and then partitioning on ~s" block venue)
            (venue-channel-put venue (make-map-request-msg block))            
            (let ((msg (sync (venue-event venue)))) ;; same as venue-channel-get              
              (log-mr-info "Received ~s from ~s" msg venue)
              (semaphore-post notify))))
  (void))

(: shuffle-rdd (All (A) (RDDFile A) -> (RDDFile A)))
(define (shuffle-rdd rddfile)
  rddfile)

(: reduce-rdd (All (A B) (RDDFile A) MapDynFn1 -> (RDDFile B)))
(define (reduce-rdd rddfile dynfn)
  rddfile)

(: map-rdd (All (A B) MapDynFn1 (RDDFile A) -> (RDDFile B)))
(define (map-rdd dynfn rddfile) 
  
  (log-mr-info "Performing map step on ~s with ~s" rddfile dynfn)
  
  
  ((inst RDDFile Void) '()))

(: map-reduce (All (A) (RDDFile A) TextParser Mapper Writer Grouper -> Void))
(define (map-reduce rddfile parser mapper writer grouper)  
  
  (define: tracker : (Tracker Block)
    (make-tracker (list->set (RDDFile-blocks rddfile))))
  
  (define: venues : (Listof Venue) (spawn-venues (todo-count tracker)))
  
  ;(let: ((rdd : (RDDFile A)   (map-rdd map-dynfn rddfile)))
  ;  (let: ((rdd : (RDDFile A) (shuffle-rdd rdd)))
  ;    (reduce-rdd rdd reduce-dynfn)
  (void))