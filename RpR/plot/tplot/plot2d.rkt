#lang typed/racket/base

(require
 (only-in racket/math
          sqr)
 (only-in "types.rkt"
     Plot-Result
	  Point-Symbol Point2d
	  Color Levels Font-Family
	  Pen-Widths Plot-Color Plot-Colors 
	  Plot-Brush-Style Plot-Brush-Styles
	  Plot-Pen-Style Plot-Pen-Styles 
	  Format Interval Alphas Anchor Labels
	  Snip% Bitmap% Bitmap-DC%
	  Renderer-Tree Renderer2d Nonrenderer
	  Discrete-Histogram-Data Stacked-Histogram-Data))

(require/typed/provide
 plot/main

 [plot (Renderer-Tree
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
	-> Plot-Result)]

 [plot-file (Renderer-Tree
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

 ;; Punt for now.  pict is from slide-show
 ;; [plot-pict (Renderer-Tree
 ;; 	      (U Path-String Output-Port)
 ;; 	      Format
 ;; 	      [#:x-min (Option Exact-Rational)]
 ;; 	      [#:x-max (Option Exact-Rational)]
 ;; 	      [#:y-min (Option Exact-Rational)]
 ;; 	      [#:y-max (Option Exact-Rational)]
 ;; 	      [#:width  Natural]
 ;; 	      [#:height Natural]
 ;; 	      [#:title (Option String)]
 ;; 	      [#:x-label (Option String)]
 ;; 	      [#:y-label (Option String)]
 ;; 	      [#:legend-anchor Anchor]
 ;; 	      -> Void)]

 [plot-bitmap (Renderer-Tree
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
	       -> (Instance Bitmap%))]

 [plot/dc (Renderer-Tree
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
 
 ;; 3.2 2D Point Renderers
 [points ((Listof Point2d)
	  [#:x-min (Option Exact-Rational)]
	  [#:x-max (Option Exact-Rational)]
	  [#:y-min (Option Exact-Rational)]
	  [#:y-max (Option Exact-Rational)] 
	  [#:color   Plot-Color]
	  [#:fill-color (U Plot-Color 'auto)]
	  [#:size Nonnegative-Real]
	  [#:line-width Positive-Real]
	  [#:alpha Nonnegative-Real]
	  [#:label (Option String)]
	  -> Renderer2d)]
 
 [error-bars ((Listof (Vector Real Real Real))
	      [#:x-min (Option Exact-Rational)]
	      [#:x-max (Option Exact-Rational)]
	      [#:y-min (Option Exact-Rational)]
	      [#:y-max (Option Exact-Rational)]
	      [#:color Color]
	      [#:line-width Nonnegative-Real]
	      [#:line-style Plot-Pen-Style]
	      [#:width Nonnegative-Real]
	      [#:alpha Nonnegative-Real]
	      -> Renderer2d)]

 
 ;; TR does not support KW and optional args.
 [function ((Real -> Real)
	    (Option Exact-Rational)
	    (Option Exact-Rational)
	    [#:y-min   Exact-Rational]
	    [#:y-max   Exact-Rational]
	    [#:samples Integer]
	    [#:color   Plot-Color]
	    [#:width   Real]
	    [#:style   Plot-Pen-Style]
	    [#:alpha   Real]
	    [#:label  (Option String)]
	    -> Renderer2d)]             

 [inverse ((Real -> Real)
	   (Option Exact-Rational)
	   (Option Exact-Rational)
	   [#:x-min   Exact-Rational]
	   [#:x-max   Exact-Rational]
	   [#:samples Integer]
	   [#:color   Plot-Color]
	   [#:width   Real]
	   [#:style   Plot-Pen-Style]
	   [#:alpha   Real]
	   [#:label  (Option String)]
	   -> Renderer2d)]

 [lines ((Listof Point2d)
	 [#:x-min (Option Exact-Rational)]
	 [#:x-max (Option Exact-Rational)]
	 [#:y-min   Exact-Rational]
	 [#:y-max   Exact-Rational]
	 [#:color   Plot-Color]
	 [#:width   Real]
	 [#:style   Plot-Pen-Style]
	 [#:alpha   Real]
	 [#:label  (Option String)]
	 -> Renderer2d)]
 
 [parametric ((Real -> Point2d)
	      Exact-Rational
	      Exact-Rational
	      [#:x-min (Option Exact-Rational)]
	      [#:x-max (Option Exact-Rational)]
	      [#:y-min   Exact-Rational]
	      [#:y-max   Exact-Rational]
	      [#:samples Integer]
	      [#:color   Plot-Color]
	      [#:width   Real]
	      [#:style   Plot-Pen-Style]
	      [#:alpha   Real]
	      [#:label  (Option String)]
	      -> Renderer2d)]
 
 [polar ((Real -> Real)
	 Real
	 Real
	 [#:x-min (Option Exact-Rational)]
	 [#:x-max (Option Exact-Rational)]
	 [#:y-min   Exact-Rational]
	 [#:y-max   Exact-Rational]
	 [#:samples Integer]
	 [#:color   Plot-Color]
	 [#:width   Real]
	 [#:style   Plot-Pen-Style]
	 [#:alpha   Real]
	 [#:label  (Option String)]
	 -> Renderer2d)]
 

 [density ((Listof Real)
	   Real	    
	   [#:x-min (Option Exact-Rational)]
	   [#:x-max (Option Exact-Rational)]
	   [#:y-min   Exact-Rational]
	   [#:y-max   Exact-Rational]
	   [#:samples Integer]
	   [#:color   Plot-Color]
	   [#:width   Real]
	   [#:style   Plot-Pen-Style]
	   [#:alpha   Real]
	   [#:label  (Option String)]
	   -> Renderer2d)]

 ;; 3.4 2D Interval Renderers

 [function-interval ((Real -> Real)
		     (Real -> Real)
		     (Option Exact-Rational)
		     (Option Exact-Rational)
		     [#:y-min   Exact-Rational]
		     [#:y-max   Exact-Rational]
		     [#:samples Integer]
		     [#:color   Plot-Color]
		     [#:style   Plot-Pen-Style]
		     [#:line1-color Plot-Color]
		     [#:line1-width Nonnegative-Real]
		     [#:line1-style Plot-Pen-Style]
		     [#:line2-color Plot-Color]
		     [#:line2-width Nonnegative-Real]
		     [#:line2-style Plot-Pen-Style]
		     [#:alpha   Real]
		     [#:label  (Option String)]
		     -> Renderer2d)]


 [inverse-interval ((Real -> Real)
		    (Real -> Real)
		    Exact-Rational
		    Exact-Rational
		    [#:x-min (Option Exact-Rational)]
		    [#:x-max (Option Exact-Rational)]
		    [#:samples Integer]
		    [#:color   Plot-Color]
		    [#:style   Plot-Pen-Style]
		    [#:line1-color Plot-Color]
		    [#:line1-width Nonnegative-Real]
		    [#:line1-style Plot-Pen-Style]
		    [#:line2-color Plot-Color]
		    [#:line2-width Nonnegative-Real]
		    [#:line2-style Plot-Pen-Style]
		    [#:alpha   Real]
		    [#:label  (Option String)]
		    -> Renderer2d)]
 

 [lines-interval ((Listof Point2d)
		  (Listof Point2d)
		  [#:x-min (Option Exact-Rational)]
		  [#:x-max (Option Exact-Rational)]
		  [#:y-min (Option Exact-Rational)]
		  [#:y-max (Option Exact-Rational)]
		  [#:color   Plot-Color]
		  [#:style   Plot-Pen-Style]
		  [#:line1-color Plot-Color]
		  [#:line1-width Nonnegative-Real]
		  [#:line1-style Plot-Pen-Style]
		  [#:line2-color Plot-Color]
		  [#:line2-width Nonnegative-Real]
		  [#:line2-style Plot-Pen-Style]
		  [#:alpha   Real]
		  [#:label  (Option String)]
		  -> Renderer2d)]


 [parametric-interval ((Listof Point2d)
		       (Listof Point2d)
		       Exact-Rational
		       Exact-Rational
		       [#:x-min (Option Exact-Rational)]
		       [#:x-max (Option Exact-Rational)]
		       [#:y-min (Option Exact-Rational)]
		       [#:y-max (Option Exact-Rational)]
		       [#:samples Integer]
		       [#:color   Plot-Color]
		       [#:style   Plot-Pen-Style]
		       [#:line1-color Plot-Color]
		       [#:line1-width Nonnegative-Real]
		       [#:line1-style Plot-Pen-Style]
		       [#:line2-color Plot-Color]
		       [#:line2-width Nonnegative-Real]
		       [#:line2-style Plot-Pen-Style]
		       [#:alpha   Real]
		       [#:label  (Option String)]
		       -> Renderer2d)]
 
 [polar-interval ((Real -> Real)
		  (Real -> Real)
		  Exact-Rational
		  Exact-Rational
		  [#:x-min (Option Exact-Rational)]
		  [#:x-max (Option Exact-Rational)]
		  [#:y-min (Option Exact-Rational)]
		  [#:y-max (Option Exact-Rational)]
		  [#:samples Integer]
		  [#:color   Plot-Color]
		  [#:style   Plot-Pen-Style]
		  [#:line1-color Plot-Color]
		  [#:line1-width Nonnegative-Real]
		  [#:line1-style Plot-Pen-Style]
		  [#:line2-color Plot-Color]
		  [#:line2-width Nonnegative-Real]
		  [#:line2-style Plot-Pen-Style]
		  [#:alpha   Real]
		  [#:label  (Option String)]
		  -> Renderer2d)]
 
 
 ;; 3.5 2D Contour (Isoline) Renderers
 
 [isoline ((Real Real -> Real)
	   Real
	   Exact-Rational
	   Exact-Rational
	   Exact-Rational
	   Exact-Rational	    
	   [#:samples Integer]
	   [#:color   Plot-Color]
	   [#:width   Nonnegative-Real]	    
	   [#:style   Plot-Pen-Style]
	   [#:alpha   Real]
	   [#:label  (Option String)]
	   -> Renderer2d)]
 

 [contours ((Real Real -> Real)
	    Exact-Rational
	    Exact-Rational
	    Exact-Rational
	    Exact-Rational	    
	    [#:samples Integer]
	    [#:levels  Levels]
	    [#:colors  (U Plot-Colors (Listof Real))]
	    [#:widths  (U Pen-Widths (Listof Real))]
	    [#:styles  (U Plot-Pen-Styles (Listof Real))]
	    [#:alphas  Alphas]
	    [#:label  (Option String)]
	    -> Renderer2d)]

 [contour-intervals ((Real Real -> Real)
		     Exact-Rational
		     Exact-Rational
		     Exact-Rational
		     Exact-Rational	    
		     [#:samples Integer]
		     [#:levels  Levels]
		     [#:colors  (U Plot-Colors (Listof Real))]
		     [#:styles  (U Plot-Pen-Styles (Listof Real))]
		     [#:contour-colors (U Plot-Colors (Listof Real))]
		     [#:contour-widths (U Pen-Widths (Listof Real))]
		     [#:contour-styles (U Plot-Pen-Styles (Listof Real))]
		     [#:alphas  Alphas]
		     [#:label  (Option String)]
		     -> Renderer2d)]
 
 ;; 3.6 2D Rectangle Renderers

 [rectangles ((Listof (Vector Interval Interval))
	      [#:x-min (Option Exact-Rational)]
	      [#:x-max (Option Exact-Rational)]
	      [#:y-min (Option Exact-Rational)]
	      [#:y-max (Option Exact-Rational)]
	      [#:color Plot-Color]
	      [#:style Plot-Brush-Style]
	      [#:line-color Plot-Color]
	      [#:line-width Plot-Brush-Style]
	      [#:line-style Plot-Pen-Style]
	      [#:alpha Nonnegative-Real]	 
	      [#:label (Option String)]
	      -> Renderer2d)]

 [area-histogram ((Real -> Real) 
		  (Listof Real)
		  [#:x-min (Option Exact-Rational)]
		  [#:x-max (Option Exact-Rational)]
		  [#:y-min (Option Exact-Rational)]
		  [#:y-max (Option Exact-Rational)]
		  [#:samples Integer]                   
		  [#:color Plot-Color]
		  [#:style Plot-Brush-Style]
		  [#:line-color Plot-Color]
		  [#:line-width Plot-Brush-Style]
		  [#:line-style Plot-Pen-Style]
		  [#:alpha Real]	 
		  [#:label (Option String)]
		  -> Renderer2d)]
 
 [discrete-histogram (Discrete-Histogram-Data
		      [#:x-min (Option Exact-Rational)]
		      [#:x-max (Option Exact-Rational)]
		      [#:y-min (Option Exact-Rational)]
		      [#:y-max (Option Exact-Rational)]
		      [#:gap Real]
		      [#:skip Real]
		      [#:invert? Boolean]
		      [#:color Plot-Color]
		      [#:style Plot-Brush-Style]
		      [#:line-color Plot-Color]
		      [#:line-width Plot-Brush-Style]
		      [#:line-style Plot-Pen-Style]
		      [#:alpha Real]
		      [#:label (Option String)]
		      [#:add-ticks? Boolean]
		      [#:far-ticks? Boolean]
		      -> Renderer2d)]
 
 [stacked-histogram (Stacked-Histogram-Data
		     [#:x-min (Option Exact-Rational)]
		     [#:x-max (Option Exact-Rational)]
		     [#:y-min (Option Exact-Rational)]
		     [#:y-max (Option Exact-Rational)]
		     [#:gap Real]
		     [#:skip Real]
		     [#:invert? Boolean]
		     [#:colors Plot-Colors]
		     [#:styles Plot-Brush-Styles]
		     [#:line-colors Plot-Colors]
		     [#:line-widths Pen-Widths]
		     [#:line-styles Plot-Pen-Styles]
		     [#:alphas (U Alphas Natural)]
		     [#:labels (U Labels Natural)]
		     [#:add-ticks? Boolean]
		     [#:far-ticks? Boolean]
		     -> (Listof Renderer2d))]
 
 ;; 3.7 2D Plot Decoration Renderers
 
 [x-axis (Real 
	  [#:ticks? Boolean]
	  [#:labels? Boolean]
	  [#:far? Boolean]
	  [#:alpha Nonnegative-Real]
	  -> Renderer2d)]
 [y-axis (Real 
	  [#:ticks? Boolean]
	  [#:labels? Boolean]
	  [#:far? Boolean]
	  [#:alpha Nonnegative-Real]
	  -> Renderer2d)]
 [axes (Real 
	Real 
	[#:x-ticks? Boolean]
	[#:y-ticks? Boolean]
	[#:x-labels? Boolean]
	[#:y-labels? Boolean]
	[#:x-alpha Nonnegative-Real]
	[#:y-alpha Nonnegative-Real]
	-> (Listof Renderer2d))]

 [polar-axes ([#:number Nonnegative-Integer]
	      [#:ticks? Boolean]
	      [#:labels? Boolean]
	      [#:alpha Nonnegative-Real]
	      -> Renderer2d)]
 [x-tick-lines (-> Renderer2d)]
 [y-tick-lines (-> Renderer2d)]
 [tick-grid (-> (Listof Renderer2d))]

 [point-label (Point2d
	       (Option String)
	       [#:color Color]
	       [#:size Nonnegative-Real]
	       [#:family Font-Family]
	       [#:anchor Anchor]
	       [#:angle Real]
	       [#:point-color Plot-Color]
	       [#:point-fill-color (U Plot-Color 'auto)]
	       [#:point-size Nonnegative-Real]
	       [#:point-line-width Nonnegative-Real]
	       [#:point-sym Point-Symbol]
	       [#:alpha Nonnegative-Real]
	       -> Renderer2d)]
 
 [function-label ((Real -> Real)
		  Real
		  (Option String)
		  [#:color Color]
		  [#:size Nonnegative-Real]
		  [#:family Font-Family]
		  [#:anchor Anchor]
		  [#:angle Real]
		  [#:point-color Plot-Color]
		  [#:point-fill-color (U Plot-Color 'auto)]
		  [#:point-size Nonnegative-Real]
		  [#:point-line-width Nonnegative-Real]
		  [#:point-sym Point-Symbol]
		  [#:alpha Nonnegative-Real]
		  -> Renderer2d)]
 
 [inverse-label ((Real -> Real)
		 Real
		 (Option String)
		 [#:color Color]
		 [#:size Nonnegative-Real]
		 [#:family Font-Family]
		 [#:anchor Anchor]
		 [#:angle Real]
		 [#:point-color Plot-Color]
		 [#:point-fill-color (U Plot-Color 'auto)]
		 [#:point-size Nonnegative-Real]
		 [#:point-line-width Nonnegative-Real]
		 [#:point-sym Point-Symbol]
		 [#:alpha Nonnegative-Real]
		 -> Renderer2d)]

 [parametric-label ((Real -> Point2d)
		    Real
		    (Option String)
		    [#:color Color]
		    [#:size Nonnegative-Real]
		    [#:family Font-Family]
		    [#:anchor Anchor]
		    [#:angle Real]
		    [#:point-color Plot-Color]
		    [#:point-fill-color (U Plot-Color 'auto)]
		    [#:point-size Nonnegative-Real]
		    [#:point-line-width Nonnegative-Real]
		    [#:point-sym Point-Symbol]
		    [#:alpha Nonnegative-Real]
		    -> Renderer2d)]


 [polar-label ((Real -> Real)
	       Real
	       (Option String)
	       [#:color Color]
	       [#:size Nonnegative-Real]
	       [#:family Font-Family]
	       [#:anchor Anchor]
	       [#:angle Real]
	       [#:point-color Plot-Color]
	       [#:point-fill-color (U Plot-Color 'auto)]
	       [#:point-size Nonnegative-Real]
	       [#:point-line-width Nonnegative-Real]
	       [#:point-sym Point-Symbol]
	       [#:alpha Nonnegative-Real]
	       -> Renderer2d)]


 )

					;(define (test)
					;  (: f (Real -> Real))
					;  (define (f x) (exp (* -1/2 (sqr x))))
					;  (plot (list (area-histogram f (linear-seq -4 4 10) 
					;                              #:label "Root 2")
					;              (function f -4 4))))
					;
					;(define (test2)
					;  (let: ((data : Discrete-Histogram-Data
					;               (list #(A 1) #(B 2) #(B 3)
					;                     (vector 'C (ivl 0.5 1.5)))))
					;    (plot (discrete-histogram data #:label "Hello"))))
					;
;; (define (test3)
;;   (let: ((h1-data : Discrete-Histogram-Data '(#(Eggs 1.5) #(Bacon 2.5) #(Pancakes 3.5)))
;;          (h2-data : Discrete-Histogram-Data '(#(Eggs 1.4) #(Bacon 2.3) #(Pancakes 3.1))))
;;     (plot (list (discrete-histogram
;;                  h1-data
;;                  #:skip 2.5 #:x-min 0
;;                  #:label "AMD")
;;                 (discrete-histogram
;;                  h2-data
;;                  #:skip 2.5 #:x-min 1
;;                  #:label "Intel" #:color 2 #:line-color 2))
;;           #:x-label "Breakfast Food" #:y-label "Cooking Time (minutes)"
;;           #:title "Cooking Times For Breakfast Food, Per Processor")))

					;(define (test4)
					;  (let: ((data : Stacked-Histogram-Data
					;               (list #(a (1 1 1)) #(b (1.5 3))
					;                     #(c ()) #(d (1/2)))))
					;    (plot (stacked-histogram data
					;                             #:invert? #t
					;                             #:labels '("Red" #f "Blue"))
					;          #:legend-anchor 'top-right)))

					;(define (test4)
					;  (parameterize ([plot-width    150]
					;                 [plot-height   150]
					;                 [plot-x-label  #f]
					;                 [plot-y-label  #f])
					;    
					;    (define: xs : (Listof Real) (build-list 20 (λ _ (random))))
					;    (define: ys : (Listof Real) (build-list 20 (λ _ (random))))
					;    (list (plot (points (map vector xs ys)))
					;          (plot (points (map vector xs ys)
					;                        #:x-min 0 #:x-max 1
					;                        #:y-min 0 #:y-max 1)))))
