(define-module (examples common)
  :use-module (hans patcher)
  :use-module (hans modules snd-io objects)
  :use-module (hans modules snd-oscillator objects)
  :use-module (hans modules snd-ringbuffer objects)
  :use-module (hans modules gfx-superformula objects)
  :use-module (hans modules gfx-quad objects)
  :use-module (hans modules gfx-scopes objects)
  :use-module (hans modules gfx-script objects)
  :use-module (hans modules gfx-sndtex objects)
  :use-module (hans modules gfx-filter objects)
  :export (settings
           hans-create
           hans-connect))

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

(define* (hans-create name #:optional args position)
  (hans 'create name (if (eq? args #f) '() args)
                     (if (eq? position #f) '(0 0) position)))

(define (hans-connect source outlet sink inlet)
  (hans 'connect source outlet sink inlet))
