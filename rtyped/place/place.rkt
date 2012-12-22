#lang typed/racket/base

;;(require typed/private/utils)

(provide
 after-seconds%
 after-seconds
 ;After-Seconds%
 Remote-Node%
 Remote-Connection%
 spawn-node-supervise-place-at)

(define-type after-seconds% (Class () () ()))
(define-type After-Seconds% (Class () () ()))
(define-type Remote-Node% (Class () () ()))
(define-type Remote-Connection% (Class () () ()))

(require 
 (only-in racket/place/distributed
          after-seconds))

(require/typed
 racket/place/distributed
 
; [spawn-node-with-place-at (String 
;                            Module-Path 
;                            Symbol
;                            [#:listen-port Natural]
;                            [#:initial-message Any]
;                            [#:racket-path Path]
;                            [#:ssh-bin-path Path]
;                            [#:distributed-launch-path Path]
;                            [#:restart-on-exit Any]
;                            [#:place-name (Option Symbol)]
;                            [#:thunk Boolean]
;                            -> Place)])
;               
 
[spawn-node-supervise-place-at (String 
                                (U Path Module-Path)
                                Symbol                                
                                [#:distributed-launch-path (Option Path)]
                                [#:initial-message Any]
                                [#:listen-port Natural]
                                [#:named (Option Symbol)]                                
                                [#:racket-path Path]
                                [#:restart-on-exit Any]
                                [#:ssh-bin-path Path]
                                [#:thunk Boolean]
                                ->
                                (Values (Instance Remote-Node%)
                                        (Instance Remote-Connection%)))])