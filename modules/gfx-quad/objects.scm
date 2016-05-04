(define-module (hans modules gfx-quad objects))
(use-modules (hans objects))

(define library "libhans.gfx.quad")
(define width  1184)
(define height 640)
(define base (dirname (current-filename)))

(define-public (gfx-quad)
  (graphics-object "gfx-quad"
    library
    "Simple quad"
    '()
    (list
      (shader 'vertex "quad/shader/vertex"
        (slurp-file (string-append base "/quad.vert")))
      (shader 'fragmnet "quad/shader/fragment"
        (slurp-file (string-append base "/quad.frag"))))
    (fbo #t
      (list
        (fbo-attachment 'color "Output data" width height 4)))))
