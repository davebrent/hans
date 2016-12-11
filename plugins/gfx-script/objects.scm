(define-module (hans plugin gfx-script objects)
  :use-module (hans objects))

(define-public (gfx-script settings args)
  (graphics-object
    "gfx-script"
    "libhans.gfx.script"
    "Execute guile code in the graphics graph"
    '()
    '()
    (fbo #t
      (list
        (fbo-attachment 'color "Output data" (assq-ref settings 'width)
                                             (assq-ref settings 'height) 4)))))
