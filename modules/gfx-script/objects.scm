(define-module (hans modules gfx-script objects))
(use-modules (hans objects)
             (hans utils))

(define library "libhans.gfx.script")
(define base (dirname (current-filename)))

(define-public (gfx-script settings args)
  (graphics-object
    "gfx-script"
    library
    "Execute guile code in the graphics graph"
    '()
    '()
    (fbo #t
      (list
        (fbo-attachment 'color "Output data" (assq-ref settings 'width)
                                             (assq-ref settings 'height) 4)))))
