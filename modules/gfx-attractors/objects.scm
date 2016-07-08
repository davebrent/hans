(define-module (hans modules gfx-attractors objects))
(use-modules (hans objects)
             (hans utils))

(define library "libhans.gfx.attractors")
(define base (dirname (current-filename)))

(define-public (gfx-attractors settings args)
  (graphics-object
    "gfx-attractors"
    library
    ""
    '()
    (list
      (shader 'vertex "attractors/shaders/vert"
        (slurp-file (string-append base "/particle.vert")))
      (shader 'fragment "attractors/shaders/frag"
        (slurp-file (string-append base "/particle.frag"))))
    (fbo #t
      (list
        (fbo-attachment 'color "Output data" (assq-ref settings 'width)
                                             (assq-ref settings 'height) 4)))))
