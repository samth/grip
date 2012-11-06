#lang typed/racket/base

(require
 "types.rkt")
   
(require/typed/provide 
 plot/main

  ;; 9.1 Compatibility
  [plot-deprecation-warnings? (Parameterof Boolean)]
  
  ;; 9.2 Output
  [plot-new-window? (Parameterof Boolean)]
  [plot-width (Parameterof Exact-Positive-Integer)]
  [plot-height (Parameterof Exact-Positive-Integer)]
  [plot-jpeg-quality (Parameterof Nonnegative-Integer)]
  [plot-ps/pdf-interactive? (Parameterof Boolean)]
  
  ;; 9.3 Appearance 
  [plot-foreground (Parameterof (Instance Color%))]
  [plot-background (Parameterof (Instance Color%))]
  [plot-foreground-alpha (Parameterof Real)]
  [plot-font-size (Parameterof Nonnegative-Real)]
  [plot-font-family (Parameterof Font-Family)]
  [plot-line-width (Parameterof Nonnegative-Real)]
  [plot-legend-anchor (Parameterof Anchor)]
  [plot-legend-box-alpha (Parameterof Nonnegative-Real)]
  [plot-tick-size (Parameterof Nonnegative-Real)]
  [plot-title (Parameterof (Option String))]
  [plot-x-label (Parameterof (Option String))]
  [plot-y-label (Parameterof (Option String))]
  [plot-x-far-label (Parameterof (Option String))]
  [plot-y-far-label (Parameterof (Option String))]
  [plot-z-far-label (Parameterof (Option String))]
  [plot-x-tick-label-anchor (Parameterof Anchor)]
  [plot-x-tick-label-angle (Parameterof Real)]
  [plot-y-tick-label-anchor (Parameterof Anchor)]
  [plot-y-tick-label-angle (Parameterof Real)]
  [plot-x-far-tick-label-anchor (Parameterof Anchor)]
  [plot-x-far-tick-label-angle (Parameterof Real)]
  [plot-y-far-tick-label-anchor (Parameterof Anchor)]
  [plot-y-far-tick-label-angle (Parameterof Real)]
  [plot-x-axis? (Parameterof Boolean)]
  [plot-x-far-axis? (Parameterof Boolean)]
  [plot-y-axis? (Parameterof Boolean)]
  [plot-y-far-axis? (Parameterof Boolean)]
  [plot-z-axis? (Parameterof Boolean)]
  [plot-z-far-axis? (Parameterof Boolean)]
  [plot-animating? (Parameterof Boolean)]
  [animated-samples (Integer -> Integer)]
  [plot-decorations? (Parameterof Boolean)]
  
  ;; 9.4 Lines
  [line-samples (Parameterof Natural)]
  [line-color (Parameterof Plot-Color)]
  [line-width (Parameterof Nonnegative-Integer)]
  [line-style (Parameterof Plot-Pen-Style)]
  [line-alpha (Parameterof Nonnegative-Real)]
  
  ;; 9.5 Intervals
  [interval-color (Parameterof Plot-Color)]
  [interval-style (Parameterof Plot-Brush-Style)]
  [interval-line1-color (Parameterof Plot-Color)]
  [interval-line1-width (Parameterof Nonnegative-Real)]
  [interval-line1-style (Parameterof Plot-Pen-Styles)]
  [interval-line2-color (Parameterof Plot-Color)]
  [interval-line2-width (Parameterof Nonnegative-Real)]
  [interval-line2-style (Parameterof Plot-Pen-Styles)]
  [interval-alpha (Parameterof Real)]
  
  ;; 9.6 Points
  [point-sym (Parameterof Point-Symbol)]
  [point-color (Parameterof Plot-Color)]
  [point-size (Parameterof Nonnegative-Real)]
  [point-line-width (Parameterof Nonnegative-Real)]
  [point-alpha (Parameterof Nonnegative-Real)]
  
  ;; 9.7 Vector Fields
  [vector-field-samples (Parameterof Exact-Positive-Integer)]
  [vector-field-color (Parameterof Plot-Color)]
  [vector-field-line-width (Parameterof Nonnegative-Real)]
  [vector-field-line-style (Parameterof Plot-Pen-Style)]
  [vector-field-scale (Parameterof (U 'auto 'normalized))]
  [vector-field-alpha (Parameterof Nonnegative-Real)]
  [vector-field3d-samples (Parameterof Exact-Positive-Integer)]
  
  ;; 9.8 Error Bars
  [error-bar-width (Parameterof Nonnegative-Real)]
  [error-bar-color (Parameterof Plot-Color)]
  [error-bar-line-width (Parameterof Nonnegative-Real)]
  [error-bar-line-style (Parameterof Plot-Pen-Style)]
  [error-bar-alpha (Parameterof Nonnegative-Real)]
  
  ;; 9.9 Contours and Contour Intervals
  [default-contour-colors ((Listof Real) ->  (Listof Plot-Color))]
  [default-contour-fill-colors ((Listof Real) -> (Listof Plot-Color))]
  [contour-samples (Parameterof Nonnegative-Integer)]
  [contour-levels (Parameterof (U 'auto Exact-Positive-Integer (Listof Real)))]
  [contour-colors (Parameterof (U Plot-Colors (Listof Real)))]
  [contour-widths (Parameterof (U Pen-Widths (Listof Real)))]
  [contour-styles (Parameterof (U Plot-Pen-Styles (Listof Real)))]
  [contour-alphas (Parameterof (U Alphas (Listof Real)))]
  [contour-interval-colors (U Plot-Colors (Listof Interval))]
  [contour-interval-styles (U Plot-Brush-Styles (Listof Interval))]
  [contour-interval-alphas (U Alphas (Listof Interval))]
  
  ;; 9.10 Rectangles
  [rectangle-color (Parameterof Plot-Color)]
  [rectangle-style (Parameterof Plot-Brush-Style)]
  [rectangle-line-color (Parameterof Plot-Color)]
  [rectangle-line-width (Parameterof Nonnegative-Real)]
  [rectangle-line-style (Parameterof Plot-Pen-Styles)]
  [rectangle-alpha (Parameterof Nonnegative-Real)]
  [rectangle3d-line-width (Parameterof Nonnegative-Real)]
  [discrete-histogram-skip (Parameterof Nonnegative-Real)]
  [discrete-histogram-invert? (Parameterof Boolean)]
  [stacked-histogram-alphas (Parameterof (U Alphas Natural))]
  [stacked-histogram-colors (Parameterof (U Plot-Colors Natural))]
  [stacked-histogram-line-styles (Parameterof (U Plot-Pen-Styles Natural))]
  [stacked-histogram-styles (U Plot-Brush-Styles Natural)]
  
  ;; 9.11 Decorations
  [x-axis-alpha (Parameterof Nonnegative-Real)]
  [y-axis-alpha (Parameterof Nonnegative-Real)]
  [z-axis-alpha (Parameterof Nonnegative-Real)]
  [x-axis-far? (Parameterof Boolean)]
  [y-axis-far? (Parameterof Boolean)]
  [z-axis-far? (Parameterof Boolean)]
  [x-axis-ticks? (Parameterof Boolean)]
  [y-axis-ticks? (Parameterof Boolean)]
  [z-axis-ticks? (Parameterof Boolean)]
  [x-axis-labels? (Parameterof Boolean)]
  [y-axis-labels? (Parameterof Boolean)]
  [z-axis-labels? (Parameterof Boolean)]
  [polar-axes-number (Parameterof Exact-Nonnegative-Integer)]
  [polar-axes-alpha (Parameterof Nonnegative-Real)]
  [polar-axes-ticks? (Parameterof Boolean)]
  [polar-axes-labels? (Parameterof Boolean)]
  [label-anchor (Parameterof Anchor)]
  [label-angle (Parameterof Real)]
  [label-alpha (Parameterof Nonnegative-Real)]
  [label-point-size (Parameterof Nonnegative-Real)]
  
  ;; 9.12 General Appearance
  [plot3d-samples (Parameterof Integer)]
  [plot3d-angle (Parameterof Real)]
  [plot3d-altitude (Parameterof Real)]
  [plot3d-ambient-light (Parameterof Nonnegative-Real)]
  [plot3d-diffuse-light? (Parameterof Boolean)]
  [plot3d-specular-light? (Parameter Boolean)]
  
  ;; 9.13 Surfaces
  [surface-color (Parameterof Plot-Color)]
  [surface-style (Parameterof Plot-Brush-Style)]
  [surface-line-color (Parameterof Plot-Color)]
  [surface-line-width (Parameterof Nonnegative-Real)]
  [surface-alpha (Parameterof Nonnegative-Real)]
  
  ;; 9.14 Contour Surfaces
  [contour-interval-line-colors (Parameterof (U Plot-Colors (Listof Interval)))]
  [contour-interval-line-widths (Parameterof (U Pen-Widths (Listof Interval)))]
  [contour-interval-line-styles (Parameterof (U Plot-Pen-Styles (Listof Interval)))]
  
  ;; 9.15 Isosurfaces
  [default-isosurface-colors ((Listof Real) -> (Listof Plot-Color))]
  [default-isosurface-line-colors ((Listof Real) -> (Listof Plot-Color))]
  [isosurface-levels (Parameterof (U 'auto Nonnegative-Integer (Listof Real)))]
  [isosurface-colors (Parameterof (U Plot-Colors (Listof Real)))]
  [isosurface-styles (Parameterof (U Plot-Brush-Styles (Listof Real)))]
  [isosurface-line-colors (Parameterof (U Plot-Colors (Listof Real)))]
  [isosurface-line-widths (U Pen-Widths (Listof Real))]
  [isosurface-line-styles (U Plot-Pen-Styles (Listof Real))]
  [isosurface-alphas (U Alphas (Listof Real))]

  ;; 7.1 Axis Transforms
  [plot-x-transform (Parameterof Axis-Transform)]
  [plot-y-transform (Parameterof Axis-Transform)]
  [plot-z-transform (Parameterof Axis-Transform)]

  ;; 7.2 Axis Ticks
  [plot-x-ticks (Parameterof Ticks)]
  [plot-x-far-ticks (Parameterof Ticks)]
  
  [plot-y-ticks (Parameterof Ticks)]
  [plot-y-far-ticks (Parameterof Ticks)]
  
  [plot-z-ticks (Parameterof Ticks)]
  [plot-z-far-ticks (Parameterof Ticks)]

  [plot-d-ticks (Parameterof Ticks)]
  [plot-r-ticks (Parameterof Ticks)]
  [ticks-default-number (Parameterof Positive-Integer)]

  [date-ticks-formats (Parameterof (Listof String))]
  [time-ticks-formats (Parameterof (Listof String))]

  [currency-ticks-scales (Parameterof (Listof String))]
  [currency-ticks-formats (Parameterof (List String String String))]

  )

