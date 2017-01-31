(define-module (hans plugin gfx-filter objects)
  :use-module (hans extension)
  :use-module (hans objects)
  :use-module (hans utils))

(define base (hans-plugin-path "gfx-filter"))

(define (effect name)
  (shader 'fragment
    (string-append "filter/shader/" name)
    (slurp-file (string-append base "/shaders/" name ".frag"))))

(display base)
(newline)

(define-public (gfx-filter settings args)
  (define width (assq-ref settings 'width))
  (define height (assq-ref settings 'height))
  (graphics-object "gfx-filter"
    "libhans.gfx.filter"
    "Effects filter"
    `(,(parameter 'amount 0.5 "The amount of filter to apply"))
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
