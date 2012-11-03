#lang typed/racket

(require
 racket/match
 (only-in syntax/location
          quote-module-path))
 ;(only-in "dynmodules.rkt"
 ;         determine-module-path
 ;         dynamic-require-modules))

(require/typed racket/place
               [opaque Place place?]
               [opaque Place-Channel place-channel?])

(define-type Remote-Node% (Class () () ()))
(define-type Remote-Connection% (Class () () ()))

(require/typed racket/place/distributed
               [start-message-router/thread ((#:listen-port Port)
                                             (#:nodes (Listof Any)) -> (Values Thread Place-Channel))]
               [spawn-nodes/join/local ((Listof (List Any)) -> (Listof (Instance Remote-Connection%)))]
               [supervise-place-at ((Instance Remote-Node%) 
                                    Module-Path 
                                    Symbol 
                                    (#:restart-on-exit Any)
                                    (#:named (U False Symbol)) 
                                    (#:thunk Boolean)
                                    -> (Instance Remote-Connection%))])

(define-type MRMsgTypes (U 'REDUCE-READY 'MAP))
(define-type MRConfiguration (List (List Any)))
(define-type MRTaskDef (Listof String))
(define-type MRNodeConfig (Listof (List Any)))

(struct: MRMsg ([msg-type : MRMsgTypes]) #:prefab)

;(: quote-module-path (-> Bytes))
;(define (quote-module-path) 
;  (string->bytes/latin-1 "/tmp"))

(: generate-place-name (Integer -> Symbol))
(define (generate-place-name id)
  (string->symbol (string-append "mpw" (number->string id))))

;(: make-map-reduce-workers (MRConfiguration -> (Listof (Instance Remote-Node%))))
;(define (make-map-reduce-workers config)
;  (define nodes (spawn-nodes/join/local config))
;  (for ([n nodes]
;        [i (in-naturals)])
;    (supervise-place-at n
;                        (determine-module-path (quote-module-path))
;                        'map-reduce-worker
;                        #:named (generate-place-name i)))
;  nodes)

;
;(: map-reduce-worker (Place-Channel -> Nothing))
;(define (map-reduce-worker ch)
;  (let loop ([map-vals null])    
;    (match (place-channel-get ch)
;      ((MRMsg 'MAP) (void)))
;    (loop null)))

;      ([list (list 'map mapper key-less-than task) rch]
;       (define nmv1 ((dynamic-require-modules mapper) task))
;       (define less-than (dynamic-require-modules key-less-than))
;       (define nmv2 (sort nmv1 less-than))
;       ;;(define nmv (map-coalesce-values nmv2))       
;       (place-channel-put rch (list 'reduce-ready))
;       (loop (cons rch nmv))))))

;(: map-reduce ((Listof Any) MRConfiguration MRTaskDef -> Void))
;(define (map-reduce nodes config tasks)
;  
;  (define-values (mrth ch)
;    (start-message-router/thread #:nodes nodes))
;  
;  (define: simple-config : MRNodeConfig
;    (for/list ([c config]
;               [i (in-naturals)])
;      (append c (list (generate-place-name i)))))
;  
;  (void))
