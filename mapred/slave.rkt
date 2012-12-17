#lang typed/racket/base

(provide 
 taskrunner-handler
 (struct-out TaskRunner))

(require 
 (for-syntax
  typed/racket  
  syntax/parse)
 racket/match
 (only-in io/iteratee/iteratee
          icomplete Iteratee)
 (only-in venue/venue
          Venue start-venue
          place-channel-get
          place-channel-put)
 (only-in "logging.rkt"
          log-mr-info)
 (only-in "types.rkt"
          Block
          DynFn DynFn-module DynFn-fn
          TextParser Mapper Writer Grouper Partition)
 (only-in "messages.rkt"
          MRInit
          StartMapPhase StartReducePhase
          TaskMsg
          MapTaskReqResp)          
 (only-in "rdd/block.rkt"
          map/text-block)
 (only-in "partition.rkt"
          partition-iteratee))

(define-syntax (dynamic-require/cast stx)
  (syntax-parse stx 
    [(_ dynfn:id fntype)
     #'(cast (dynamic-require (DynFn-module dynfn) (DynFn-fn dynfn)) fntype)]))

(struct: TaskRunner ([venue : Venue]))

(: map-phase-completion-response (Place-Channel (Partition Any) -> Void))
(define (map-phase-completion-response ch partitions)
  (void))

;; A Slave in the mapper state accepts *only* map block requests 
;; and complete map phase msg.  Upon completion returns the parition
;; into which all the map requests were processed to.
(: mapper-state-request-handler (Place-Channel TextParser Mapper Writer Grouper Index -> (Partition Any)))
(define (mapper-state-request-handler ch parser mapper writer partitioner partition-count)
  
  (define: partition-iter : (Iteratee Any (Partition Any)) (partition-iteratee writer partitioner partition-count))
  
  (let: loop : (Partition Any) ((msg : Any (place-channel-get ch)))
    (match msg
      [(and mtr (MapTaskReqResp loc sod eod))
       (map/text-block (Block loc sod eod) parser mapper partition-iter)
       (place-channel-put ch (mapper mtr))
       (loop (place-channel-get ch))]
      [(StartReducePhase )
       (icomplete partition-iter)])))

(: taskrunner-handler (Place-Channel -> Void))
(define (taskrunner-handler ch)  
  (log-mr-info "Slave ~s awaiting MR initialization.")
  (let: loop : Void ((msg : Any (place-channel-get ch)))    
    (match msg
      [(MRInit parser writer mapper sorter grouper partitions)
       (log-mr-info "Slave initialized with ~s, ~s, ~s, ~s, ~s, ~s" parser writer mapper sorter grouper partitions)
       (let: ((parser  : TextParser (dynamic-require/cast parser TextParser))
              (mapper  : Mapper     (dynamic-require/cast mapper Mapper ))
              (writer  : Writer     (dynamic-require/cast writer Writer ))
              (grouper : Grouper    (dynamic-require/cast grouper Grouper)))
         (let ((partitions (mapper-state-request-handler ch parser mapper writer grouper partitions)))
           (map-phase-completion-response ch partitions)))]
      [else (log-mr-info "Slave MUST be first initialized with MRInit message")])
    (loop (place-channel-get ch))))

