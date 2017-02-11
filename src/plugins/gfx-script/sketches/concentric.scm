(use-modules (srfi srfi-1))

(define (random-range minimum maximum)
  (+ minimum (random (- maximum minimum))))

(define (make-speed)
  (+ (/ (random 1000) 5000) 0.001))

(define (make-circles)
  `(,(random-range 0 640)  ;; x
    ,(random-range 0 360)  ;; y
    ,(random-range 1 12)   ;; num
    ,(random-range 10 1280) ;; radius
    ,(random-range 10 50)  ;; interval
    ,(random-range 1 20)   ;; stroke-width
    ,(random 255)          ;; r
    ,(random 255)          ;; g
    ,(random 255)          ;; b
    ,(random 255)))        ;; a

(define* (make-frame #:optional previous)
  (let ((from (if (eq? #f previous) (make-circles) previous)))
    `(,(make-speed)        ;; speed
      0                    ;; step
      ,from                ;; from
      ,(make-circles))))   ;; to

(define (lerp a b t)
  (+ a (* t (- b a))))

(define (lerp-all from to t)
  (map (lambda (pair)
         (lerp (car pair) (last pair) t))
       (zip from to)))

(define (draw-circles iteration circles)
  (let ((radius   (list-ref circles 3))
        (interval (list-ref circles 4))
        (width    (list-ref circles 5))
        (red      (list-ref circles 6))
        (green    (list-ref circles 7))
        (blue     (list-ref circles 8))
        (alpha    (list-ref circles 9)))
    (stroke-width width)
    (stroke red 255 255 alpha)
    (ellipse 0 0 (- radius (* iteration interval))
                 (- radius (* iteration interval)))

    (if (> iteration 0)
      (draw-circles (- iteration 1) circles))))

(define (update-frame frame)
  (let ((speed   (list-ref frame 0))
        (step    (list-ref frame 1))
        (current (list-ref frame 2))
        (future  (list-ref frame 3)))
  (save)
  (translate (list-ref current 0) (list-ref current 1))
  (draw-circles (list-ref current 2) current)
  (restore)

  (if (> step 1)
    (make-frame current)
    (begin
      (list-set! frame 2 (lerp-all current future speed))
      (list-set! frame 1 (+ speed step))
      frame))))

(let ((frames (map (lambda (_) (make-frame)) (make-list 3))))
  (size 640 360)
  (draw (lambda ()
    (background 22 22 22 255)
    (no-fill)
    (set! frames (map update-frame frames)))))
