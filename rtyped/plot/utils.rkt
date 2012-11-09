#lang typed/racket/base

;; Utilities

(require 
 (only-in "types.rkt"
	  ColorRGB Mapped-Function
	  Axis-Transform
	  Point2d Point3d
	  Plot-Color Plot-Time
	  Plot-Pen-Style Plot-Brush-Style
	  Color Interval))

(require/typed/provide plot/utils

 ;;(define-type make-mapped-function ((Any -> Any) ((Listof Any) -> (Listof Any)) -> Mapped-Function)]
 
 ;; 8.1 Formatting
 [digits-for-range (case-> (Real Real -> Integer)
			   (Real Real Integer -> Integer)
			   (Real Real Integer Integer -> Integer))]
 
 [real->plot-label (case-> (Real Integer -> Any)
			   (Real Integer Boolean -> Any))]

 [ivl->plot-label (case-> (Interval -> String)
			  (Interval Integer -> String))]

 [->plot-label (case-> (Any -> String)
		       (Any Integer -> String))]

 [real->string/trunc (Real Integer -> String)]

 [real->decimal-string* (case-> (Real Integer -> String)
				(Real Integer Integer -> String))]

 [integer->superscript (Integer -> String)]

 ;; 8.2 Sampling

 [linear-seq (Real Real Nonnegative-Integer [#:start? Boolean] [#:end? Boolean] -> (Listof Real))]

 [linear-seq* ((Listof Real) Integer [#:start? Boolean] [#:end? Boolean] -> (Listof Real))]

 [nonlinear-seq (Real Real Integer Axis-Transform 
		      [#:start? Boolean] [#:end? Boolean] -> (Listof Real))]

 [kde ((Listof Real) Real -> (U Mapped-Function (Option Exact-Rational) (Option Exact-Rational)))]

 ;; 8.3 Plot Colors and Styles
 
 [color-seq (Color Color Nonnegative-Integer [#:start? Boolean] [#:end? Boolean] 
		   -> (Listof (List Real Real Real)))]

 [color-seq* ((Listof Color) Nonnegative-Integer [#:start? Boolean] [#:end? Boolean]
	      -> (Listof ColorRGB))]

 [->color (Color -> ColorRGB)]
 [->pen-color (Plot-Color -> ColorRGB)]
 [->brush-color (Plot-Color -> ColorRGB)]
 [->pen-style (Plot-Pen-Style -> Symbol)]
 [->brush-style (Plot-Brush-Style -> Symbol)]
 
 ;; Plot-Specific Math
 [polar->cartesian (Real Real -> Point2d)]
 [3d-polar->3d-cartesian (Real Real Real -> Point3d)]
 [ceiling-log/base (Positive-Integer Nonnegative-Real -> Integer)]
 [floor-log/base (Positive-Integer Nonnegative-Real -> Integer)]
 [maybe-inexact->exact ((Option Number) -> (Option Exact-Rational))]

 ;; 8.4 Vector Functions
 [v+ ((Vectorof Real) (Vectorof Real) -> (Vectorof Real))]
 [v- ((Vectorof Real) (Vectorof Real) -> (Vectorof Real))]
 [vneg ((Vectorof Real) -> (Vectorof Real))]
 [v* ((Vectorof Real) Real -> (Vectorof Real))]
 [v/ ((Vectorof Real) Real -> (Vectorof Real))]
 [v= ((Vectorof Real) (Vectorof Real) -> (Vectorof Real))]
 [vcross ((Vectorof Real) (Vectorof Real) -> (Vector Real Real Real))]
 [vcross2 ((Vectorof Real) (Vectorof Real) -> (Vector Real Real Real))]
 [vdot ((Vector Real) (Vector Real) -> Real)]
 [vmag^2 ((Vector Real) -> Real)]
 [vnormalize ((Vectorof Real) -> (Vectorof Real))]
 [vcenter ((Listof (Vectorof Real)) -> (Vectorof Real))]
 [vrational? ((Vectorof Real) -> Boolean)]
 
 ;; 8.4 Intervals
 
 [rational-ivl? (Any -> Boolean)]
 [bounds->intervals ((Listof Real) -> (Listof Interval))]
 [clamp-real (Real Interval -> Real)]

 ;; 8.5 Dates and Times

 ;; Punt for now
 ;; [datetime->real ((U Plot-Time Date Date* SQL-Date SQL-Time SQL-Timestamp)
 ;;		  -> Real)]

 [plot-time->seconds (Plot-Time -> Real)]
 [seconds->plot-time (Real -> Plot-Time)]
 
)



