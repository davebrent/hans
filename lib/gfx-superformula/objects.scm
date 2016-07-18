(define-module (hans lib gfx-superformula objects))
(use-modules (hans objects)
             (hans utils))

(define library "libhans.gfx.superformula")
(define pi      3.14159265)
(define half-pi 1.57079632)
(define base (dirname (current-filename)))

(define-public (gfx-superformula settings args)
  (define width (assq-ref settings 'width))
  (define height (assq-ref settings 'height))
  (graphics-object "gfx-superformula"
    library
    "A study into formulated bodies"
    (list
      (parameter 'm "The 'm' value in the formula" 2 '(5.7 10))
      (parameter 'n1 "The 'n1' value in the formula" 2 '(0.5 0.3))
      (parameter 'n2 "The 'n2' value in the formula" 2 '(1.0 0.2))
      (parameter 'n2 "The 'n2' value in the formula" 2 '(1.0 0.2))
      (parameter 'n3 "The 'n3' value in the formula" 2 '(2.5 1))
      (parameter 'a "The 'a' value in the formula" 2 '(1 1))
      (parameter 'b "The 'b' value in the formula" 2 '(1 1))
      (parameter 'u "The 'u' value in the formula" 2 `(,(- pi) ,pi))
      (parameter 'v "The 'v' value in the formula" 2 `(,(- half-pi) ,half-pi))
      (parameter 'u_color "The 'color' of the body" 3 '(0.85 0.9 0.95))
      (parameter 'scale "The scale of the shape" 1 '(0.5))
      (parameter 'segments "The number of segments in the shape" 1 '(90))
      (parameter 'seed "The random value to deform the shape" 1 '(0))
      (parameter 'deform "The amount to deform the shape" 1 '(0))
      (parameter 'translate "The shapes translation" 3 '(0 0 -1))
      (parameter 'rotation_speed "The shapes rotation speed" 1 '(0.005))
      (parameter 'rotation_axis "The shapes rotation speed" 3 '(1 1 1))
      (parameter 'draw_mode "The GL mode to use to draw the shape" 1 '(0)))
    (list
      (shader 'vertex
        "superformula/shader/vertex"
        (slurp-file (string-append base "/superformula.vert")))
      (shader 'fragment
        "superformula/shader/fragment"
        (slurp-file (string-append base "/superformula.frag"))))
    (fbo #t
      (list
        (fbo-attachment 'depth "Scene depth data" width height 4)
        (fbo-attachment 'color "Scene color data" width height 4)
        (fbo-attachment 'color "Scene normal data" width height 4)))))
