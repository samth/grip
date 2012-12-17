#lang typed/racket

(: mp Module-Path)
(define mp (string->path "/code/racketlib/mapred/master.rkt"))

(: f (Integer -> Integer))
(define f (cast (dynamic-require mp 'double) (Integer -> Integer)))