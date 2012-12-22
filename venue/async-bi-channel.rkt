#lang typed/racket/base

(provide make-async-bi-channel
         async-bi-channel-put
         async-bi-channel-get
         async-bi-channel?)

(define-type Message Any)

(: make-async-channel (-> (Values Thread (Channelof Message))))
(define (make-async-channel)
  (define: ch : (Channelof Message) (make-channel))
  (values
   (thread
    (lambda ()
      (let loop ()
        (let:  ([v : Message (thread-receive)])
          (channel-put ch v)
          (loop)))))
   ch))

(struct: AsyncBiChannel ([in :  Thread]
                         [out : (Channelof Message)])
  #:property prop:evt (λ (this) (AsyncBiChannel-out this)))

(define async-bi-channel? AsyncBiChannel?)

(: make-async-bi-channel (-> (Values AsyncBiChannel AsyncBiChannel)))
(define (make-async-bi-channel)
  (let-values (((ch1s ch1r) (make-async-channel))
               ((ch2s ch2r) (make-async-channel)))
    (values
     (AsyncBiChannel ch1s ch2r)
     (AsyncBiChannel ch2s ch1r))))

(: async-bi-channel-put (AsyncBiChannel Message -> Void))
(define (async-bi-channel-put ch msg)
  (void (thread-send (AsyncBiChannel-in ch) msg #f)))

(: async-bi-channel-get (AsyncBiChannel -> Message))
(define (async-bi-channel-get ch)
  ((inst channel-get Message) (AsyncBiChannel-out ch)))