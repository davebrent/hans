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
    (list
      (parameter 'a "Formula constant" 1 '(0.08))
      (parameter 'b "Formula constant" 1 '(1.39))
      (parameter 'c "Formula constant" 1 '(0.08))
      (parameter 'd "Formula constant" 1 '(1.03))
      (parameter 'e "Formula constant" 1 '(1.37))
      (parameter 'f "Formula constant" 1 '(0.43)))
    (list
      (shader 'vertex "attractors/shaders/vert"
        (slurp-file (string-append base "/particle.vert")))
      (shader 'fragment "attractors/shaders/frag"
        (slurp-file (string-append base "/particle.frag"))))
    (fbo #t
      (list
        (fbo-attachment 'color "Output data" (assq-ref settings 'width)
                                             (assq-ref settings 'height) 4)))))
