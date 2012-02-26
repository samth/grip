#lang typed/racket/base

(provide 
 list-queues)

(require
 (only-in (planet knozama/webkit:1/web/http/header)
          Headers make-header)
 (only-in "invoke.rkt"
	  SQSError sqs-invoke))

(: sqs-list-request ((Option String) -> Headers))
(define (sqs-list-request name-prefix)
  (if name-prefix
      (list (make-header "QueueNamePrefix" name-prefix))
      '()))

(: list-queues ((Option String) -> (U SQSError Void)))
(define (list-queues prefix)
  (sqs-invoke "/" 'ListQueues (sqs-list-request prefix)))
    
  
