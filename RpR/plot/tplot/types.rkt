#lang typed/racket/base

(provide (all-defined-out)
	 ivl Interval
	 Snip% Color% Bitmap% Bitmap-DC%)

(require
 (only-in typed/mred/mred
 	  Snip% Bitmap% Bitmap-DC% Color%))

(require/typed
 plot/utils
 [struct mapped-function ([f : (Any -> Any)]
			  [fmap : ((Listof Any) -> (Listof Any))])])

(require/typed 
 plot/main
 [opaque Interval ivl?]
 [ivl (Real Real -> Interval)]
 [struct plot-time ([second : Nonnegative-Integer]
		    [minute : Nonnegative-Integer]
		    [hour   : Nonnegative-Integer]
		    [day    : Integer])]
 [struct invertible-function ([f : (Real -> Real)]
			      [g : (Real -> Real)])])

(define-type Plot-Time plot-time)

(define-type Mapped-Function mapped-function)

(define-type Format  
  (U 'auto 'png 'jpeg 
     'xmb 'xpm 'bmp 
     'ps 'pdf 'svg))

(define-type Point-Symbol 
  (U 'dot               'point            'pixel
     'plus              'times            'asterisk
     '5asterisk         'odot             'oplus
     'otimes            'oasterisk        'o5asterisk
     'circle            'square           'diamond
     'triangle          'fullcircle       'fullsquare
     'fulldiamond       'fulltriangle     'triangleup
     'triangledown      'triangleleft     'triangleright
     'fulltriangleup    'fulltriangledown 'fulltriangleleft
     'fulltriangleright 'rightarrow       'leftarrow
     'uparrow           'downarrow        '4star
     '5star             '6star            '7star
     '8star             'full4star        'full5star
     'full6star         'full7star        'full8star
     'circle1           'circle2          'circle3
     'circle4           'circle5          'circle6
     'circle7           'circle8          'bullet
     'fullcircle1       'fullcircle2      'fullcircle3
     'fullcircle4       'fullcircle5      'fullcircle6
     'fullcircle7       'fullcircle8))

(define-type Anchor
  (U 'top-left 'top 'top-right
     'left 'center 'right
     'bottom-left 'bottom 'bottom-right))

(define-type Point2d (Vector Real Real))
(define-type Point3d (Vector Real Real Real))

(define-type Labels
  (U (Listof (Option String))
     ((Listof Any) -> (Listof (Option String)))))

(define-type Alphas
  (U (Listof Positive-Real)
     ((Listof Any) -> (Listof Real))))

(define-type Font-Family
  (U 'default 'decorative 'roman 'script 
     'swiss 'modern 'symbol 'system))

(define-type ColorRGB (List Real Real Real))

(define-type Color
  (U ColorRGB String Symbol (Instance Color%)))

(define-type Plot-Brush-Style
  (U Integer 'transparent 'solid
     'bdiagonal-hatch 'fdiagonal-hatch 'crossdiag-hatch
     'horizontal-hatch 'vertical-hatch 'cross-hatch))

(define-type Plot-Brush-Styles
  (U (Listof Plot-Pen-Style)
     ((Listof Any) -> (Listof Plot-Pen-Style))))

(define-type Plot-Pen-Style 
  (U Integer 'transparent 'solid
     'bdiagnol-hatch 'fdiagonal-hatch
     'horizontal-hath 'vertical-hatch
     'crossdiag-hatch 'cross-hatch))

(define-type Plot-Pen-Styles
  (U (Listof Plot-Pen-Style)
     ((Listof Any) -> (Listof Plot-Pen-Style))))

(define-type Pen-Widths (U (Listof Plot-Pen-Style)
                           ((Listof Any) -> (Listof Plot-Pen-Style))))

(define-type Plot-Color (U Integer (List Real Real Real)
                           String Symbol (Instance Color%)))

(define-type Plot-Colors (U (Listof Plot-Color)
                            ((Listof Any) -> (Listof Plot-Color))))

(define-type Plot-Result (U Void (Instance Snip%)))

;; 7.x Axis Transformas and Tics

(define-type Invertible-Function invertible-function)

(define-type Axis-Transform (Real Real Invertible-Function -> Invertible-Function))

(require/typed
 plot/main
 [struct pre-tick ([value  : Real]
		   [major? : Boolean])]
 [struct (tick pre-tick) ([label : String])])

(define-type Pre-Tick pre-tick)

(define-type Ticks-Layout (Real Real -> (Listof Pre-Tick)))
(define-type Ticks-Format (Real Real (Listof Pre-Tick) -> (Listof String)))

(require/typed
 plot/main

 [struct ticks ([layout : Ticks-Layout]
		[format : Ticks-Format])])

(define-type Tick tick)
(define-type Ticks ticks)
(define-type Levels (U 'auto Positive-Integer (Listof Real)))

(require/typed
 plot/utils
 [opaque Renderer2d renderer2d?]
 [opaque Renderer3d renderer3d?]
 [opaque Nonrenderer nonrenderer?])

(define-type Renderer-Tree (Rec RTree (U (U Renderer2d Renderer3d Nonrenderer) (Listof RTree))))

(define-type Discrete-Histogram-Data (Listof (U (Vector Any (Option (U Real Interval)))
					   (List   Any (Option (U Real Interval))))))

(define-type Stacked-Histogram-Data (Listof (U (Vector Any (Option (U Real Interval)))
					  (List   Any (Option (U Real Interval))))))

