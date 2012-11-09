#lang typed/racket/base

(require
 (only-in "types.rkt"
	  Ticks Tick 
	  Ticks-Layout Ticks-Format
	  Levels	  
	  Invertible-Function
	  Axis-Transform))

(require/typed/provide
 plot/main

 ;; 7.1 Axis Transforms
  [id-transform Axis-Transform]
  [log-transform Axis-Transform]

  [stretch-transform (Real Real Nonnegative-Real -> Axis-Transform)]
  [collapse-transform (Real Real -> Axis-Transform)]
  [cbrt-transform Axis-Transform]
  [hand-drawn-transform (Nonnegative-Real -> Axis-Transform)]
  [axis-transform-append (Axis-Transform Axis-Transform Real -> Axis-Transform)]
  [axis-transform-bound (Axis-Transform Real Real -> Axis-Transform)]
  [axis-transform-compose (Axis-Transform Axis-Transform -> Axis-Transform)]
  [make-axis-transform (Invertible-Function -> Axis-Transform)]
  [apply-axis-transform (Axis-Transform Real Real -> Invertible-Function)]
  
  ;; 7.2 Axis Ticks
  [contour-ticks (Ticks Real Real Levels Boolean -> (Listof Tick))]
  [linear-ticks-layout ([#:number Positive-Integer]
			[#:base Positive-Integer]
			[#:divisors (Listof Positive-Integer)]
			-> Ticks-Layout)]

  [linear-ticks-format (-> Ticks-Format)]
  [linear-ticks ([#:number Positive-Integer]
		 [#:base Positive-Integer]
		 [#:divisors (Listof Positive-Integer)]
		 -> Ticks)]

  [log-ticks-layout ([#:number Exact-Positive-Integer]
		     [#:base Positive-Integer] -> Ticks-Layout)]
  [log-ticks-format ([#:base Positive-Integer] -> Ticks-Format)]
  [log-ticks ([#:number Positive-Integer]
	     [#:base Positive-Integer] 
	     -> Ticks)]

  [date-ticks-layout ([#:number Positive-Integer] -> Ticks-Layout)]
  [date-ticks-format ([#:formats (Listof String)] -> Ticks-Format)]
  [date-ticks ([#:number Positive-Integer]
	       [#:formats (Listof String)]
	       -> Ticks)]

  [time-ticks-layout ([#:number Positive-Integer] -> Ticks-Layout)]
  [time-ticks-format ([#:formats (Listof String)] -> Ticks-Format)]
  [time-ticks ([#:number Positive-Integer]
	       [#:formats (Listof String)]
	       -> Ticks)]
  
		      
  [currency-ticks-format ([#:kind (U String Symbol)]
			  [#:scales (Listof String)]
			  [#:formats (List String String String)]
			  -> Ticks-Format)]

  [currency-ticks ([#:number Positive-Integer]
		   [#:kind (U String Symbol)]
		   [#:scales (Listof String)]
		   [#:formats (List String String String)]
		   -> Ticks-Format)]

  [us-currency-scales (Listof String)]
  [uk-currency-scales (Listof String)]
  [eu-currency-scales (Listof String)]
  [us-currency-formats (List String String String)]
  [uk-currency-formats (List String String String)]
  [eu-currency-formats (List String String String)]

  [no-ticks-layout Ticks-Layout]
  [no-ticks-format Ticks-Format]
  [no-ticks Ticks]

  [bit/byte-ticks-format ([#:size (U 'byte 'bit)]
			  [#:kind (U 'CS 'SI)]
			  -> Ticks-Format)]
  [bit/byte-ticks ([#:number Exact-Positive-Integer]
		   [#:size (U 'byte 'bit)]
		   [#:kind (U 'CS 'SI)]
		   -> Ticks)]
  [fraction-ticks-format ([#:base Positive-Integer]
			  [#:divisors (Listof Positive-Integer)]
			  -> Ticks-Format)]
  [fraction-ticks ([#:base Positive-Integer]
		   [#:divisors (Listof Positive-Integer)]
		   -> Ticks)]

  [ticks-mimic ((-> Ticks) -> Ticks)]
  [ticks-add (case-> (Ticks (Listof Real) -> Ticks)
		     (Ticks (Listof Real) Boolean -> Ticks))]
  [ticks-scale (Ticks Invertible-Function -> Ticks)]

  [id-function Invertible-Function]
  [invertible-compose (Invertible-Function Invertible-Function -> Invertible-Function)]
  [invertible-inverse (Invertible-Function -> Invertible-Function)]
  [linear-scale (case-> (Exact-Rational -> Invertible-Function)
			(Exact-Rational Exact-Rational -> Invertible-Function))]  
)
 




