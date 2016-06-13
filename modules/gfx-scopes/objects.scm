(define-module (hans modules gfx-scopes objects))
(use-modules (hans objects)
             (hans utils))

(define library "libhans.gfx.scopes")
(define base (dirname (current-filename)))

(define-public (gfx-oscilloscope settings args)
  (graphics-object
    "gfx-oscilloscope"
    library
    "Display a signals waveform"
    '()
    (list
      (shader 'vertex "scopes/shaders/oscilloscope"
        (slurp-file (string-append base "/oscilloscope.vert")))
      (shader 'fragment "scopes/shaders/fragment"
        (slurp-file (string-append base "/oscilloscope.frag"))))
    (fbo #t
      (list
        (fbo-attachment 'color "Output data" (assq-ref settings 'width)
                                             (assq-ref settings 'height) 4)))))

(define-public (gfx-phasescope settings args)
  (graphics-object
    "gfx-phasescope"
    library
    "Phasescope"
    '()
    (list
      (shader 'vertex "scopes/shaders/phasescope"
        (slurp-file (string-append base "/phasescope.vert")))
      (shader 'fragment "scopes/shaders/fragment"
        (slurp-file (string-append base "/phasescope.frag"))))
    (fbo #t
      (list
        (fbo-attachment 'color "Output data" (assq-ref settings 'width)
                                             (assq-ref settings 'height) 4)))))
