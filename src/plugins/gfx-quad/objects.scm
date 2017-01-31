(define-module (hans plugin gfx-quad objects)
  :use-module (hans extension)
  :use-module (hans objects)
  :use-module (hans utils))

(define base (hans-plugin-path "gfx-quad"))

(define-public (gfx-quad settings args)
  (define width (assq-ref settings 'width))
  (define height (assq-ref settings 'height))
  (graphics-object
    "gfx-quad"
    "libhans.gfx.quad"
    "Simple quad"
    '()
    (list
      (shader 'vertex "quad/shader/vertex"
        (slurp-file (string-append base "/quad.vert")))
      (shader 'fragment "quad/shader/fragment"
        (slurp-file (string-append base "/quad.frag"))))
    (fbo #t
      (list
        (fbo-attachment 'color "Output data" width height 4)))))
