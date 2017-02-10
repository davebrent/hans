(define-module (hans plugin gfx-noise objects)
  :use-module (hans objects))

(define-public (gfx-perlin settings args)
  (graphics-object
    "gfx-perlin"
    "libhans.gfx.noise"
    "Simple noise"
    `(,(parameter 'frequency 10
         "Number of cycles per unit length that the noise function outputs")
      ,(parameter 'lacunarity 2
         "Multiplier determining how quickly frequency increases for each successive octave")
      ,(parameter 'octavecount 8
         "Number of octaves in the noise function"))
    '()
    '()))
