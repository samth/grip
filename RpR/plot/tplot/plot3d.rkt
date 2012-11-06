#lang typed/racket/base

(require
 (only-in "types.rkt"
	  Renderer3d
	  Levels Interval Labels
	  Snip% Bitmap-DC%
	  Format Anchor 
	  Point3d Point-Symbol
	  Pen-Widths Alphas
	  Plot-Color Plot-Colors
	  Plot-Pen-Style Plot-Pen-Styles
	  Plot-Brush-Style Plot-Brush-Styles
	  Renderer-Tree))

(require/typed/provide
 plot/main

 ;; 4.0 3D Plot Procedures
 
 [plot3d (Renderer-Tree
	  [#:x-min (Option Exact-Rational)]
	  [#:x-max (Option Exact-Rational)]
	  [#:y-min (Option Exact-Rational)]
	  [#:y-max (Option Exact-Rational)]
	  [#:width  Natural]
	  [#:height Natural]
	  [#:title (Option String)]
	  [#:x-label (Option String)]
	  [#:y-label (Option String)]
	  [#:legend-anchor Anchor]
	  [#:out-file (Option (U Path Output-Port))]
	  [#:out-kind Format]
	  -> (U (Instance Snip%) Void))]

 [plot3d-file (Renderer-Tree
	       (U Path-String Output-Port)
	       Format
	       [#:x-min (Option Exact-Rational)]
	       [#:x-max (Option Exact-Rational)]
	       [#:y-min (Option Exact-Rational)]
	       [#:y-max (Option Exact-Rational)]
	       [#:width  Natural]
	       [#:height Natural]
	       [#:title (Option String)]
	       [#:x-label (Option String)]
	       [#:y-label (Option String)]
	       [#:legend-anchor Anchor]
	     -> Void)]
 
 [plot3d/dc (Renderer-Tree
	     Bitmap-DC%
	     Real
	     Real
	     Nonnegative-Real
	     Nonnegative-Real
	     [#:x-min (Option Exact-Rational)]
	     [#:x-max (Option Exact-Rational)]
	     [#:y-min (Option Exact-Rational)]
	     [#:y-max (Option Exact-Rational)]
	     [#:title (Option String)]
	     [#:x-label (Option String)]
	     [#:y-label (Option String)]
	     [#:legend-anchor Anchor]
	     -> Void)]

 ;; 5.0 3D Renderers
 
 [points3d ((Listof Point3d)
	    [#:x-min (Option Exact-Rational)]
	    [#:x-max (Option Exact-Rational)]
	    [#:y-min (Option Exact-Rational)]
	    [#:y-max (Option Exact-Rational)] 
	    [#:z-min (Option Exact-Rational)]
	    [#:z-max (Option Exact-Rational)] 
	    [#:sym Point-Symbol]
	    [#:color   Plot-Color]
	    [#:fill-color (U Plot-Color 'auto)]
	    [#:size Nonnegative-Real]
	    [#:line-width Positive-Real]
	    [#:alpha Nonnegative-Real]
	    [#:label (Option String)]
	    -> Renderer3d)]

 [vector-field3d ((U (Real Real Real -> Point3d)
		     (Point3d -> Point3d))
		  (Option Exact-Rational)
		  (Option Exact-Rational)
		  (Option Exact-Rational)
		  (Option Exact-Rational) 
		  (Option Exact-Rational)
		  (Option Exact-Rational) 
		  [#:samples Nonnegative-Integer]
		  [#:scale (U Real 'auto 'normalized)]
		  [#:color   Plot-Color]
		  [#:line-width Nonnegative-Real]
		  [#:line-style Plot-Pen-Style]
		  [#:alpha Nonnegative-Real]
		  [#:label (Option String)]
		  -> Renderer3d)]

 [lines3d ((Listof Point3d)
	   [#:x-min (Option Exact-Rational)]
	   [#:x-max (Option Exact-Rational)]
	   [#:y-min (Option Exact-Rational)]
	   [#:y-max (Option Exact-Rational)] 
	   [#:z-min (Option Exact-Rational)]
	   [#:z-max (Option Exact-Rational)] 
	   [#:color   Plot-Color]
	   [#:width Nonnegative-Real]
	   [#:style Plot-Pen-Style]
	   [#:alpha Nonnegative-Real]
	   [#:label (Option String)]
	   -> Renderer3d)]
 
 [parametric3d ((Real -> Point3d)
		Exact-Rational
		Exact-Rational
 		[#:x-min (Option Exact-Rational)]
		[#:x-max (Option Exact-Rational)]
		[#:y-min (Option Exact-Rational)]
		[#:y-max (Option Exact-Rational)] 
		[#:z-min (Option Exact-Rational)]
		[#:z-max (Option Exact-Rational)] 
		[#:samples Nonnegative-Integer]
		[#:color   Plot-Color]
		[#:width Nonnegative-Real]
		[#:style Plot-Pen-Style]
		[#:alpha Nonnegative-Real]
		[#:label (Option String)]
		-> Renderer3d)]

 ;; 5.4 3D Surface Renderers

 [surface3d ((Real Real -> Real)
	     (Option Exact-Rational)
	     (Option Exact-Rational)
	     (Option Exact-Rational)
	     (Option Exact-Rational)
	     [#:z-min (Option Exact-Rational)]
	     [#:z-max (Option Exact-Rational)]
	     [#:samples Nonnegative-Integer]
	     [#:color   Plot-Color]	     
	     [#:style Plot-Pen-Style]
	     [#:line-color Plot-Color]
	     [#:line-width Nonnegative-Real]
	     [#:line-style Plot-Pen-Style]
	     [#:alpha Nonnegative-Real]
	     [#:label (Option String)]
	     -> Renderer3d)]
	     
 [polar3d ((Real Real -> Real)
	   [#:x-min (Option Exact-Rational)]
	   [#:x-max (Option Exact-Rational)]
	   [#:y-min (Option Exact-Rational)]
	   [#:y-max (Option Exact-Rational)]
	   [#:z-min (Option Exact-Rational)]
	   [#:z-max (Option Exact-Rational)]
	   [#:samples Nonnegative-Integer]
	   [#:color   Plot-Color]	     
	   [#:style Plot-Brush-Style]
	   [#:line-color Plot-Color]
	   [#:line-width Nonnegative-Real]
	   [#:line-style Plot-Pen-Style]
	   [#:alpha Nonnegative-Real]
	   [#:label (Option String)]
	   -> Renderer3d)]

 [isoline3d ((Real Real -> Real)
	     Real
	     (Option Exact-Rational)
	     (Option Exact-Rational)
	     (Option Exact-Rational)
	     (Option Exact-Rational)
	     [#:z-min (Option Exact-Rational)]
	     [#:z-max (Option Exact-Rational)]
	     [#:samples Nonnegative-Integer]
	     [#:color   Plot-Color]	     
	     [#:width Real]
	     [#:style Plot-Pen-Style]
	     [#:alpha Nonnegative-Real]
	     [#:label (Option String)]
	     -> Renderer3d)]

 [contours3d ((Real Real -> Real)
	     (Option Exact-Rational)
	     (Option Exact-Rational)
	     (Option Exact-Rational)
	     (Option Exact-Rational)
	     [#:z-min (Option Exact-Rational)]
	     [#:z-max (Option Exact-Rational)]
	     [#:samples Nonnegative-Integer]
	     [#:colors   Plot-Colors]  
	     [#:widths (U Pen-Widths (Listof Real))]
	     [#:styles (U Plot-Pen-Styles (Listof Real))]
	     [#:alphas (U Alphas (Listof Real))]
	     [#:label (Option String)]
	     -> Renderer3d)]
	     

 [contour-intervals3d ((Real Real -> Real)
		       (Option Exact-Rational)
		       (Option Exact-Rational)
		       (Option Exact-Rational)
		       (Option Exact-Rational)
		       [#:z-min (Option Exact-Rational)]
		       [#:z-max (Option Exact-Rational)]
		       [#:samples Nonnegative-Integer]
		       [#:levels Levels]
		       [#:colors   Plot-Colors]  
		       [#:styles (U Plot-Brush-Styles (Listof Real))]
		       [#:line-colors Plot-Colors]
		       [#:line-widths Pen-Widths]
		       [#:line-styles Plot-Pen-Styles]
		       [#:contour-colors (U Plot-Colors (Listof Real))]
		       [#:contour-widths (U Plot-Colors (Listof Real))]
		       [#:contour-styles (U Plot-Pen-Styles (Listof Real))]
		       [#:alphas (U Alphas (Listof Interval))]
		       [#:label (Option String)]
		       -> Renderer3d)]

 [isosurface3d ((Real Real Real -> Real)		
		(Option Exact-Rational)
		(Option Exact-Rational)
		(Option Exact-Rational)
		(Option Exact-Rational)
		(Option Exact-Rational)
		(Option Exact-Rational)
		[#:samples Nonnegative-Integer]
		[#:color   Plot-Color]  
		[#:style Plot-Brush-Style]
		[#:line-color Plot-Color]		
		[#:line-width Nonnegative-Real]
		[#:line-style Plot-Pen-Style]
		[#:alpha Nonnegative-Real]
		[#:label (Option String)]
		-> Renderer3d)]

 ;; 3D Rectangle Renderers

 [rectangles3d ((Listof (Vector Interval Interval Interval))
		[#:x-min (Option Exact-Rational)]
		[#:x-max (Option Exact-Rational)]
		[#:y-min (Option Exact-Rational)]
		[#:y-max (Option Exact-Rational)]
		[#:z-min (Option Exact-Rational)]
		[#:z-max (Option Exact-Rational)]
		[#:color Plot-Color]
		[#:style Plot-Brush-Style]
		[#:line-color Plot-Color]
		[#:line-width Plot-Brush-Style]
		[#:line-style Plot-Pen-Style]
		[#:alpha Nonnegative-Real]	 
		[#:label (Option String)]
		-> Renderer3d)]

 [discrete-histogram3d ((Listof (Vector Any Any (Option (U Real Interval))))
			[#:x-min (Option Exact-Rational)]
			[#:x-max (Option Exact-Rational)]
			[#:y-min (Option Exact-Rational)]
			[#:y-max (Option Exact-Rational)]
			[#:z-min (Option Exact-Rational)]
			[#:z-max (Option Exact-Rational)]
			[#:gap Real]
			[#:color Plot-Color]
			[#:style Plot-Brush-Style]
			[#:line-color Plot-Color]
			[#:line-width Plot-Brush-Style]
			[#:line-style Plot-Pen-Style]
			[#:alpha Real]
			[#:label (Option String)]
			[#:add-x-ticks? Boolean]
			[#:add-y-ticks? Boolean]
			[#:x-far-ticks? Boolean]
			[#:y-far-ticks? Boolean]
			-> Renderer3d)]

 [stacked-histogram3d ((Listof (Vector Any Any (Listof Real)))
		       [#:x-min (Option Exact-Rational)]
		       [#:x-max (Option Exact-Rational)]
		       [#:y-min (Option Exact-Rational)]
		       [#:y-max (Option Exact-Rational)]
		       [#:z-min (Option Exact-Rational)]
		       [#:z-max (Option Exact-Rational)]
		       [#:gap Real]
		       [#:colors Plot-Colors]
		       [#:styles Plot-Brush-Styles]
		       [#:line-colors Plot-Colors]
		       [#:line-widths Pen-Widths]
		       [#:line-styles Plot-Pen-Styles]
		       [#:alphas (U Alphas Natural)]
		       [#:labels (U Labels Natural)]
		       [#:add-x-ticks? Boolean]
		       [#:add-y-ticks? Boolean]
		       [#:x-far-ticks? Boolean]
		       [#:y-far-ticks? Boolean]
		       -> (Listof Renderer3d))]
 

)
