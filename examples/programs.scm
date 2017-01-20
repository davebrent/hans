(add-to-load-path ".")
(use-modules (hans compiler)
             (hans common)
             (hans engine)
             (hans patcher)
             (hans os)
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
        (hans-connect attractors 0 window 0))
      (make-modulation
        (hans-modulate attractors 'a 0 attractors 'b 0 0 1)
        (hans-modulate attractors 'b 0 attractors 'a 0 0 1)))))

(define (make-pgm-script name sketch)
  ;; Creating graphics with processing like functions
  (let ((script (hans-create 'gfx-script `((path . ,sketch))))
        (window (hans-create 'gfx-quad)))
    (make-program name
      (make-graphics-graph
        (hans-connect script 0 window 0)))))

(define (make-pgm-fft name buff-name)
  ;; Real time FFT
  (let ((adc  (hans-create 'snd-in `((channel . 0))))
        (osc (hans-create 'snd-oscillator))
        (ring (hans-create 'snd-ringbuffer `((name . ,buff-name))))
        (fft  (hans-create 'snd-fft))
        (ifft (hans-create 'snd-ifft))
        (dac  (hans-create 'snd-out `((channel . 0))))
        (window (hans-create 'gfx-quad))
        (scope (hans-create 'gfx-oscilloscope `((left . ,buff-name)
                                                (right . ,buff-name)))))
    (make-program name
      (make-audio-graph
        (hans-connect adc 0 fft 0)
        (hans-connect fft 0 ifft 0)
        (hans-connect fft 1 ifft 1)
        (hans-connect ifft 0 ring 0)
        (hans-connect ring 0 dac 0))
      (make-graphics-graph
        (hans-connect scope 0 window 0)))))

(define (make-pgm-feature name method)
  ;; Real time feature extraction
  (let ((osc  (hans-create 'snd-oscillator))
        (feat (hans-create 'snd-feature `((method . ,method))))
        (dac  (hans-create 'snd-out `((channel . 0)))))
    (make-program name
      (make-audio-graph
        (hans-connect osc 0 feat 0)
        (hans-connect feat 0 dac 0)))))

(define (base filename)
  (os-path-join (dirname (current-filename)) filename))

(define (dump-file ng-data)
  (let f (open-file path "w")
    (display (hans-object-stringify ng-data) f)
    (close-output-port f)))

(define ng-data (hans-compile settings
                  `(,(make-pgm-fx "dotscreen" "filter/shader/dotscreen")
                    ,(make-pgm-attractors "attractors")
                    ,(make-pgm-fx "cga" "filter/shader/cgadisplay")
                    ,(make-pgm-script "script" (base "sketches/concentric.scm"))
                    ,(make-pgm-scope "oscilloscope" "rb-foobar-2")
                    ,(make-pgm-feature "feature" "centroid")
                    ,(make-pgm-fft "fft" "rb-buff")
                    ,(make-pgm-fx "greyscale" "filter/shader/greyscale")
                    ,(make-pgm-sine "sine")
                    ,(make-pgm-pass "passthrough")
                    ,(make-pgm-ringbuffer "ringbuffer" "rb-foobar"))))

(let ((engine (make-hans-object 'engine `(,ng-data))))
  (engine-open engine)
  (engine-run engine)
  (engine-close engine))
