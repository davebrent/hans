(define-module (hans modules gfx-filter objects))
(use-modules (hans objects)
             (hans utils))

(define library "libhans.gfx.filter")
(define base (dirname (current-filename)))

(define (effect name)
  (shader 'fragment
    (string-append "filter/shader/" name)
    (slurp-file (string-append base "/shaders/" name ".frag"))))

(define-public (gfx-filter settings args)
  (define width (assq-ref settings 'width))
  (define height (assq-ref settings 'height))
  (graphics-object "gfx-filter"
    library
    "Effects filter"
    (list
      (parameter 'amount "The amount of filter to apply" 1 '(0.5)))
    (list
      (effect "gamma")
      (effect "sepia")
      (effect "invert")
      (effect "sobel")
      (effect "cgadisplay")
      (effect "dotscreen")
      (effect "greyscale")
      (effect "halftone")
      (effect "pixelate")
      (effect "rgbsplit")
      (effect "zoomblur")
      (effect "passthrough")
      (shader 'vertex "filter/shader/quad"
        (slurp-file (string-append base "/shaders/quad.vert"))))
    (fbo #t
      (list
        (fbo-attachment 'color "Filter output data" width height 4)))))
