(define-module (hans modules gfx-quad objects))
(use-modules (hans objects)
             (hans utils))

(define library "libhans.gfx.quad")
(define base (dirname (current-filename)))

(define-public (gfx-quad settings args)
  (define width (assq-ref settings 'width))
  (define height (assq-ref settings 'height))
  (graphics-object "gfx-quad"
    library
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
