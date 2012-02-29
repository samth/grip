#lang typed/racket/base

(provide
 bitwise-first-bit-set
 bitwise-ior
 bitwise-and
 bitwise-arithmetic-shift-right
 bitwise-arithmetic-shift-left
 bitwise-xor)

(require/typed 
 rnrs/arithmetic/bitwise-6
 (bitwise-first-bit-set (Integer -> Integer))
 (bitwise-ior (Integer Integer -> Integer))
 (bitwise-xor (Integer Integer -> Integer))
 (bitwise-and (Integer Integer -> Integer))
 (bitwise-arithmetic-shift-left (Integer Integer -> Integer))
 (bitwise-arithmetic-shift-right (Integer Integer -> Integer)))

