;; Based on http://koaning.io/fluctuating-repetition.html

(define width 640)
(define height 360)

(define a 0.08)
(define b 1.39)
(define c 0.08)
(define d 1.03)
(define e 1.37)
(define f 0.43)

(define (draw-point xs ys zs)
  (let ((new-x (- (+ (sin (* a xs)) (sin (* b ys))) (cos (* c zs))))
        (new-y (- (+ (sin (* d xs)) (sin (* e ys))) (cos (* f zs))))
        (new-z (+ zs 0.1)))
    (fill 255 255 255 (* 5 (+ 10 new-z)))
    (rect (+ (/ (* xs width) 6) (/ width 2))
          (+ (/ (* ys height) 6) (/ height 2)) 2 2)
    `(,new-x ,new-y ,new-z)))

(define (amount scale)
  (* (- (/ (random 10000) 5000) 1.0) scale))

(define (draw-all n)
  (let ((xs 0)
        (ys 0)
        (zs 0))
    (do ((i 1 (1+ i))) ((> i n))
      (let ((res (draw-point xs ys zs)))
        (set! xs (list-ref res 0))
        (set! ys (list-ref res 1))
        (set! zs (list-ref res 2))))))

(size width height)

(draw (lambda ()
  (background 0 0 0 255)
  (draw-all 512)
  (set! a (+ a (amount 0.01)))
  (set! b (+ b (amount 0.01)))
  (set! c (+ c (amount 0.01)))
  (set! d (+ d (amount 0.01)))
  (set! e (+ e (amount 0.01)))
  (set! f (+ f (amount 0.01)))))
