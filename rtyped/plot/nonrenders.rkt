#lang typed/racket/base

(require 
 (only-in "types.rkt"
	  Tick
	  Nonrenderer))

(require/typed/provide
 plot/main

 ;; 6.0 Nonrenders
 
 [x-ticks ((Listof Tick)
	   [#:far? Boolean]
	   -> Nonrenderer)]
 
 [y-ticks ((Listof Tick)
	   [#:far? Boolean]
	   -> Nonrenderer)]

 [z-ticks ((Listof Tick)
	   [#:far? Boolean]
	   -> Nonrenderer)]

 [invisible-rect ((Option Exact-Rational)
		  (Option Exact-Rational)
		  (Option Exact-Rational)
		  (Option Exact-Rational)
		  -> Nonrenderer)]

 [invisible-rect3d ((Option Exact-Rational)
		  (Option Exact-Rational)
		  (Option Exact-Rational)
		  (Option Exact-Rational)
		  (Option Exact-Rational)
		  (Option Exact-Rational)
		  -> Nonrenderer)]

)
 
