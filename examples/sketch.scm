(define mouse-x 0)
(define mouse-y 0)
(define angle 0)

(load-font "sans" "../etc/Tahoma-Regular.ttf")
(load-font "bold" "../etc/Tahoma-Bold.ttf")

(size 640 360)

;(mouse-move (lambda (x y)
;  (set! mouse-x x)
;  (set! mouse-y y)))

(draw (lambda ()
  (background 200 200 200 255)

  (save)
  (translate (- 240 50) (- 160 50))
  (rotate angle)

  (text-font "bold")
  (fill 0 0 0 255)
  (text-size 24)
  (text 50 200 "Hello World")

  (no-stroke)
  (fill 255 255 255 255)
  (rect 0 0 100 100)

  (translate 100 100)
  (fill 255 0 0 255)
  (no-fill)
  (stroke 0 0 0 255)
  (ellipse 0 0 100 100)
  (restore)

  (stroke 255 0 0 255)
  (stroke-width 10)
  (line mouse-x mouse-y 240 160)
  (set! angle (+ angle 0.01))))
