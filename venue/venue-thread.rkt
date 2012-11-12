#lang typed/racket/base

(require
 '#%boot ;; for orig-paramz
 ;; This a "native" module built in the C runtime with low-level Place API.
 '#%place-struct
 ;; ditto
 (only-in '#%paramz parameterization-key make-custodian-from-main))

#| 
Venues are a direct translation of Racket Places, just native in Typed Racket.
I anticipate 3 levels of Venue abstraction.
 1. Venue abstraction of between threads and channels.
 2. Venue abstraction between single process isolated venues and place-channels on the same node.
 3. Venue abstraction between two different nodes.

In this file we define the thread/channel level abstraction to a Venue.
|#

;; FIXME RPR - Type this to be allowable Place types.
(define-type Message Any)

(define-type BreakMode (Option (U 'hang-up 'terminate)))

(struct: VenueThread 
  ([thread    : Thread]
   [channel   : Place-Channel]
   [custodian : Custodian])
  #:property prop:evt (λ (vt) (TH-place-channel-in (VenueThread vt))))

;; To make this a typed message channel, i.e of (Channelof T),
;; probably need to use a macro or use overhead of assertion predicate and 
;; some sort of error logging.  Consider #{x : T} notation. 
(: make-async-channel-and-receive-thread (-> (Values (Channelof Any) Thread)))
(define (make-async-channel-and-receive-thread)
  (let: ((ch : (Channelof Any) (make-channel)))
    (values
     ch
     (thread (λ ()
               (let loop ()
                 (let: ([v : Any (thread-receive)])
                   (channel-put ch v)
                   (loop))))))))

(: venue-thread-channels (-> (Values Place-Channel Place-Channel)))
(define (venue-thread-channels)
  (let-values (((a-ch a-th) (make-async-channel-and-receive-thread))
               ((b-ch b-th) (make-async-channel-and-receive-thread)))
    (let ((a-vt (TH-place-channel a-th a-ch))
          (b-vt (TH-place-channel b-th b-ch)))
      (values a-vt b-vt))))

(: venue-thread-put (VenueThread Message -> Void))
(define (venue-thread-put venue msg)
  (void (thread-send (TH-place-channel-out (VenueThread-channel venue))
                     msg #f)))

(: venue-thread-get (VenueThread -> Message))
(define (venue-thread-get venue)
  ((inst channel-get Any) (TH-place-channel-in (VenueThread-channel venue))))
    
(: venue-thread-sleep (Natural -> Void))
(define (venue-thread-sleep n) (sleep n))

(: venue-thread-wait (VenueThread -> Void))
(define (venue-thread-wait venue)
  (thread-wait (VenueThread-thread venue)))

(: venue-thread-kill (VenueThread -> Void))
(define (venue-thread-kill venue)
  (custodian-shutdown-all (VenueThread-custodian venue)))

(: venue-thread-break (case-> (VenueThread -> Void)
                              (VenueThread (Option BreakMode) -> Void)))
(define (venue-thread-break venue [mode #f])
  (break-thread (VenueThread-thread venue) mode))

;; FIXME RPR - Event types are a TODO in base-env.rkt
;(: venue-thread-dead-evt (VenueThread -> Event))
;(define (venue-thread-dead-evt venue)
;  (thread-dead-evt (VenueThread-thread venue)))


(define-type StartFn (Place-Channel -> Void))

(: dynamic-venue-thread ((U Module-Path Resolved-Module-Path) Symbol -> VenueThread))
(define (dynamic-venue-thread module start-fn)
  (let-values:  ((([ch-a : Place-Channel][ch-b : Place-Channel]) (venue-thread-channels)))
    (let:  ((cust : Custodian (make-custodian-from-main)))
      (let: ((th : Thread (thread (λ ()
                                    (with-continuation-mark parameterization-key
                                      orig-paramz
                                      (parameterize ([current-namespace (make-base-namespace)]
                                                     [current-custodian cust])
                                        ((cast ((inst dynamic-require Void) module start-fn) StartFn) ch-b)))))))
        (VenueThread th ch-a cust)))))
                            