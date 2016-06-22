(use-modules (hans compiler)
             (hans patcher)
             (hans modules snd-io objects)
             (hans modules snd-oscillator objects)
             (hans modules snd-ringbuffer objects)
             (hans modules gfx-superformula objects)
             (hans modules gfx-quad objects)
             (hans modules gfx-scopes objects)
             (hans modules gfx-script objects)
             (hans modules gfx-sndtex objects)
             (hans modules gfx-filter objects))

(define settings `(
  (width     . 640)
  (height    . 360)
  (blocksize . 64)
  (channels  . 2)))

(define hans (make-environment settings
  `((gfx-quad         . ,gfx-quad)
    (gfx-oscilloscope . ,gfx-oscilloscope)
    (gfx-phasescope   . ,gfx-phasescope)
    (gfx-script       . ,gfx-script)
    (gfx-sndtex       . ,gfx-sndtex)
    (gfx-superformula . ,gfx-superformula)
    (gfx-filter       . ,gfx-filter)
    (snd-oscillator   . ,snd-oscillator)
    (snd-ringbuffer   . ,snd-ringbuffer)
    (snd-in           . ,snd-in)
    (snd-out          . ,snd-out))))

(define superformula (hans 'create 'gfx-superformula '() '(0 0)))

(define (make-program name shader)
  (let ((effect (hans 'create 'gfx-filter `((name . ,shader)) '(0 0)))
        (window (hans 'create 'gfx-quad '() '(0 0))))
    (hans-program name
      (make-audio-graph)
      (make-graphics-graph
        (hans 'connect superformula 0 effect 0)
        (hans 'connect effect 0 window 0)))))

(define (make-sine-audio name)
  (let ((osc (hans 'create 'snd-oscillator `((channels . 2)) '(0 0)))
        (dac (hans 'create 'snd-out `((channel . 0) (channel . 1)) '(0 0))))
    (hans-program name
      (make-audio-graph
        (hans 'connect osc 0 dac 0)
        (hans 'connect osc 1 dac 1))
      (make-graphics-graph))))

(define (make-passthrough-audio name)
  (let ((adc (hans 'create 'snd-in `((channel . 0) (channel . 1)) '(0 0)))
        (dac (hans 'create 'snd-out `((channel . 0) (channel . 1)) '(0 0))))
    (hans-program name
      (make-audio-graph
        (hans 'connect adc 0 dac 0)
        (hans 'connect adc 1 dac 1))
      (make-graphics-graph))))

(define (make-ringbuffer-audio name buff-name)
  (let ((adc    (hans 'create 'snd-in         `((channel . 0))       '(0 0)))
        (ring   (hans 'create 'snd-ringbuffer `((name . ,buff-name)) '(0 0)))
        (dac    (hans 'create 'snd-out        `((channel . 0))       '(0 0)))
        (reader (hans 'create 'gfx-sndtex     `((name . ,buff-name)) '(0 0)))
        (window (hans 'create 'gfx-quad       '()                    '(0 0))))
    (hans-program name
      (make-audio-graph
        (hans 'connect adc 0 ring 0)
        (hans 'connect ring 0 dac 0))
      (make-graphics-graph
        (hans 'connect reader 0 window 0)))))

(define (make-oscscope-audio name buff-name)
  (let ((adc    (hans 'create 'snd-in           `((channel . 0))       '(0 0)))
        (ring   (hans 'create 'snd-ringbuffer   `((name . ,buff-name)) '(0 0)))
        (dac    (hans 'create 'snd-out          `((channel . 0))       '(0 0)))
        (scope  (hans 'create 'gfx-oscilloscope `((left . ,buff-name)
                                                  (right . ,buff-name)) '(0 0)))
        (window (hans 'create 'gfx-quad         '()                    '(0 0))))
    (hans-program name
      (make-audio-graph
        (hans 'connect adc 0 ring 0)
        (hans 'connect ring 0 dac 0))
      (make-graphics-graph
        (hans 'connect scope 0 window 0)))))

(define (make-script-program name)
  (let ((script (hans 'create 'gfx-script `((path . "../examples/sketch.scm")) '(0 0)))
        (effect (hans 'create 'gfx-filter `((name . "filter/shader/dotscreen")) '(0 0)))
        (window (hans 'create 'gfx-quad '() '(0 0))))
    (hans-program name
      (make-audio-graph)
      (make-graphics-graph
        (hans 'connect script 0 window 0)))))

(let ((programs (list (make-script-program "script")
                      (make-oscscope-audio "oscilloscope" "rb-foobar-2")
                      (make-program "cgadisplay" "filter/shader/cgadisplay")
                      (make-program "dotscreen" "filter/shader/dotscreen")
                      (make-program "greyscale" "filter/shader/greyscale")
                      (make-sine-audio "audio-01")
                      (make-passthrough-audio "audio-02")
                      (make-ringbuffer-audio "audio-03" "rb-foobar"))))
  (hans-compile (hans-file programs) '(
    (output        . "super.hans")
    (library-paths . ("/Users/dave/Projects/hans/build/lib"
                      "/vagrant/build/lib")))))
