(define-module (hans plugin gfx-attractors objects)
  :use-module (hans objects)
  :use-module (hans extension)
  :use-module (hans utils))

(define base (hans-plugin-path "gfx-attractors"))

(define-public (gfx-attractors settings args)
  (graphics-object
    "gfx-attractors"
    "libhans.gfx.attractors"
    ""
    `(,(parameter 'a 0.08)
      ,(parameter 'b 1.39)
      ,(parameter 'c 0.08)
      ,(parameter 'd 1.03)
      ,(parameter 'e 1.37)
      ,(parameter 'f 0.43))
    (list
      (shader 'vertex "attractors/shaders/vert"
        (slurp-file (string-append base "/particle.vert")))
      (shader 'fragment "attractors/shaders/frag"
        (slurp-file (string-append base "/particle.frag"))))
    (fbo #t
      (list
        (fbo-attachment 'color "Output data" (assq-ref settings 'width)
                                             (assq-ref settings 'height) 4)))))
