(define-module (hans plugin gfx-superformula objects)
  :use-module (hans objects)
  :use-module (hans utils))

(define pi      3.14159265)
(define half-pi 1.57079632)
(define base (dirname (current-filename)))

(define-public (gfx-superformula settings args)
  (define width (assq-ref settings 'width))
  (define height (assq-ref settings 'height))
  (graphics-object "gfx-superformula"
    "libhans.gfx.superformula"
    "A study into formulated bodies"
   `(,(parameter 'm '(5.7 10))
     ,(parameter 'n1 '(0.5 0.3))
     ,(parameter 'n2 '(1.0 0.2))
     ,(parameter 'n2 '(1.0 0.2))
     ,(parameter 'n3 '(2.5 1))
     ,(parameter 'a '(1 1))
     ,(parameter 'b '(1 1))
     ,(parameter 'u `(,(- pi) ,pi))
     ,(parameter 'v `(,(- half-pi) ,half-pi))
     ,(parameter 'u_color '(0.85 0.9 0.95) "The 'color' of the body")
     ,(parameter 'scale 0.5 "Scale of the shape")
     ,(parameter 'segments 90 "Number of segments in the shape")
     ,(parameter 'seed 0 "Seed for random shape deformation")
     ,(parameter 'deform 0 "Shape deform amount")
     ,(parameter 'translate '(0 0 -1) "Shape translation")
     ,(parameter 'rotation_speed 0.005 "The shapes rotation speed")
     ,(parameter 'rotation_axis '(1 1 1) "The shapes rotation speed")
     ,(parameter 'draw_mode 0 "GL draw mode"))
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
