(add-to-load-path ".")
(use-modules (hans compiler)
             (hans patcher)
             (examples common))

;; Objects may be shared among programs
(define superformula (hans-create 'gfx-superformula))

(define (make-pgm-fx name shader)
  ;; Apply a filter to another graphics object
  (let ((effect (hans-create 'gfx-filter `((name . ,shader))))
        (window (hans-create 'gfx-quad)))
    (make-program name
      (make-graphics-graph
        (hans-connect superformula 0 effect 0)
        (hans-connect effect 0 window 0)))))

(define (make-pgm-sine name)
  ;; Creating audio signals
  (let ((osc (hans-create 'snd-oscillator `((channels . 2))))
        (dac (hans-create 'snd-out `((channel . 0) (channel . 1)))))
    (make-program name
      (make-audio-graph
        (hans-connect osc 0 dac 0)
        (hans-connect osc 1 dac 1)))))

(define (make-pgm-pass name)
  ;; Passing audio from microphone to speakers
  (let ((adc (hans-create 'snd-in `((channel . 0) (channel . 1))))
        (dac (hans-create 'snd-out `((channel . 0) (channel . 1)))))
    (make-program name
      (make-audio-graph
        (hans-connect adc 0 dac 0)
        (hans-connect adc 1 dac 1)))))

(define (make-pgm-ringbuffer name buff-name)
  ;; Sharing audio data with a graphics object
  (let ((adc    (hans-create 'snd-in `((channel . 0))))
        (ring   (hans-create 'snd-ringbuffer `((name . ,buff-name))))
        (dac    (hans-create 'snd-out `((channel . 0))))
        (reader (hans-create 'gfx-sndtex `((name . ,buff-name))))
        (window (hans-create 'gfx-quad)))
    (make-program name
      (make-audio-graph
        (hans-connect adc 0 ring 0)
        (hans-connect ring 0 dac 0))
      (make-graphics-graph
        (hans-connect reader 0 window 0)))))

(define (make-pgm-scope name buff-name)
  ;; Displaying an audio signal
  (let ((adc   (hans-create 'snd-in `((channel . 0))))
        (ring  (hans-create 'snd-ringbuffer `((name . ,buff-name))))
        (dac   (hans-create 'snd-out `((channel . 0))))
        (scope (hans-create 'gfx-oscilloscope `((left . ,buff-name)
                                                (right . ,buff-name))))
        (window (hans-create 'gfx-quad)))
    (make-program name
      (make-audio-graph
        (hans-connect adc 0 ring 0)
        (hans-connect ring 0 dac 0))
      (make-graphics-graph
        (hans-connect scope 0 window 0)))))

(define (make-pgm-attractors name)
  ;; Strange attractors object
  (let ((attractors (hans-create 'gfx-attractors))
        (window (hans-create 'gfx-quad)))
    (make-program name
      (make-graphics-graph
        (hans-connect attractors 0 window 0)))))

(define (make-pgm-script name sketch)
  ;; Creating graphics with processing like functions
  (let ((script (hans-create 'gfx-script `((path . ,sketch))))
        (window (hans-create 'gfx-quad)))
    (make-program name
      (make-graphics-graph
        (hans-connect script 0 window 0)))))

(hans-compile
  (hans-file (list (make-pgm-attractors "attractors")
                   ;(make-pgm-script "script"
                   ;                 "../examples/sketches/fluctuating.scm")
                   (make-pgm-scope "oscilloscope" "rb-foobar-2")
                   (make-pgm-fx "cga" "filter/shader/cgadisplay")
                   (make-pgm-fx "dotscreen" "filter/shader/dotscreen")
                   (make-pgm-fx "greyscale" "filter/shader/greyscale")
                   (make-pgm-sine "sine")
                   (make-pgm-pass "passthrough")
                   (make-pgm-ringbuffer "ringbuffer" "rb-foobar")))
  '((output        . "programs.hans")
    (library-paths . ("/Users/dave/Projects/hans/build/lib"
                      "/usr/lib"))))
