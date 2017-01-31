(define-module (hans plugin gfx-sndtex objects)
  :use-module (hans objects))

(define-public (gfx-sndtex settings args)
  (graphics-object
    "gfx-sndtex"
    "libhans.gfx.sndtex"
    "Transform a ring buffer into a texture"
    '()
    '()
    '()))
