#lang typed/racket/base

(provide
 Venue
 place-channel-get
 place-channel-put
 venue-channel-get
 venue-channel-put
 start-venue)

(require
 racket/pretty
 (prefix-in RT- (only-in '#%place
                         place-channel-put
                         place-channel-get
                         place-dead-evt
                         place-pumper-threads
                         dynamic-place))
 (only-in "streams.rkt"
          if-stream-out
          if-stream-in
          pump-ports))

;; From the point of view of a user of a venue.
;; i.e., we see a sink for msgs to a venue as input in which to 'sink' the msgs.
(define-type Venue-Sink (U Thread (Option Input-Port)))
(define-type Venue-Source (U Thread (Option Output-Port)))

(struct: Venue ([place : Place]                
                [inp   : Venue-Sink]
                [outp  : Venue-Source]
                [errp  : Venue-Source]) #:transparent)

(define-type XVenue (U Place Place-Channel))
(define-type Venue-Channel Place-Channel)

(: pump-place (Place Output-Port (Option Input-Port)  (Option Input-Port) 
                     Input-Port  (Option Output-Port) Output-Port ->                      
                     (Values (U Thread (Option Output-Port))
                             (U Thread (Option Input-Port))
                             (U Thread (Option Input-Port)))))
(define (pump-place p pin pout perr in out err)
  (let-values ([(t-in t-out t-err) (pump-ports (RT-place-dead-evt p) pin pout perr in out err)])
    (RT-place-pumper-threads p (vector t-in t-out t-err))))

(: start-venue (Symbol Module-Path Symbol 
                       (Option Input-Port)
                       (Option Output-Port)
                       (Option Output-Port) -> Venue))                       
(define (start-venue who module start-fn in out err)
  (let-values (([venue pin pout perr] (RT-dynamic-place module start-fn 
                                                        (if-stream-in who in)
                                                        (if-stream-out who out)
                                                        (if-stream-out who err))))
    (Venue venue 
           (and (not in)  pin)
           (and (not out) pout) 
           (and (not err) perr))))

(: place-channel-get (Place-Channel -> Any))
(define place-channel-get RT-place-channel-get)

(: place-channel-put (Place-Channel Any -> Void))
(define place-channel-put RT-place-channel-put)

(: venue-channel-get (Venue -> Any))
(define (venue-channel-get venue)
  (RT-place-channel-get (Venue-place venue)))

(: venue-channel-put (Venue Any -> Void))
(define (venue-channel-put venue value)
  (RT-place-channel-put (Venue-place venue) value))