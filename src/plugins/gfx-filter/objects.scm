(define-module (hans plugin gfx-filter objects)
  :use-module (hans extension)
  :use-module (hans objects)
  :use-module (hans utils))

(define base (hans-plugin-path "gfx-filter"))

(define-public (gfx-filter settings args)
  (graphics-object "gfx-filter"
    "libhans.gfx.filter"
    "Effects filter"
    `(,(parameter 'amount 0.1 "The amount of filter to apply")
      ,(parameter 'filter 7 "The selected filter"))
    `(,(shader 'fragment "filter/shader/frag"
               (slurp-file (string-append base "/filter.frag.glsl")))
      ,(shader 'vertex "filter/shader/vert"
               (slurp-file (string-append base "/filter.vert.glsl"))))
    (fbo #t
         `(,(fbo-attachment 'color "Filter output data"
                            (assq-ref settings 'width)
                            (assq-ref settings 'height) 4)))))
