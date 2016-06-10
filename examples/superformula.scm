(use-modules (hans compiler)
             (hans patcher)
             (hans modules snd-io objects)
             (hans modules snd-oscillator objects)
             (hans modules snd-ringbuffer objects)
             (hans modules gfx-superformula objects)
             (hans modules gfx-quad objects)
             (hans modules gfx-filter objects))

(define settings `(
  (width     . 1184)
  (height    . 640)
  (blocksize . 256)
  (channels  . 2)))

(define hans (make-environment settings
  `((gfx-quad         . ,gfx-quad)
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

(define (make-ringbuffer-audio name)
  (let ((adc (hans 'create 'snd-in `((channel . 0)) '(0 0)))
        (ring (hans 'create 'snd-ringbuffer `((name . "foo")) '(0 0))))
    (hans-program name
      (make-audio-graph
        (hans 'connect adc 0 ring 0))
      (make-graphics-graph))))

(let ((programs (list (make-program "cgadisplay" "filter/shader/cgadisplay")
                      (make-program "dotscreen" "filter/shader/dotscreen")
                      (make-program "greyscale" "filter/shader/greyscale")
                      (make-sine-audio "audio-01")
                      (make-passthrough-audio "audio-02")
                      (make-ringbuffer-audio "audio-03"))))
  (hans-compile (hans-file programs) '(
    (output        . "superformula.hans")
    (library-paths . ("/Users/dave/Projects/hans/build/lib"
                      "/vagrant/build/lib")))))
