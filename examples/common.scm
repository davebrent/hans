(define-module (examples common)
  :use-module (hans patcher)
  :use-module (hans plugin snd-analysis objects)
  :use-module (hans plugin snd-io objects)
  :use-module (hans plugin snd-oscillator objects)
  :use-module (hans plugin snd-ringbuffer objects)
  :use-module (hans plugin gfx-attractors objects)
  :use-module (hans plugin gfx-superformula objects)
  :use-module (hans plugin gfx-quad objects)
  :use-module (hans plugin gfx-scopes objects)
  :use-module (hans plugin gfx-script objects)
  :use-module (hans plugin gfx-sndtex objects)
  :use-module (hans plugin gfx-filter objects)
  :export (settings
           hans-create
           hans-connect
           hans-modulate))

(define settings `(
  (width      . 640)
  (height     . 360)
  (blocksize  . 128)
  (samplerate . 48000)
  (channels   . 2)))

(define hans (make-environment settings
  `((gfx-quad         . ,gfx-quad)
    (gfx-oscilloscope . ,gfx-oscilloscope)
    (gfx-phasescope   . ,gfx-phasescope)
    (gfx-script       . ,gfx-script)
    (gfx-attractors   . ,gfx-attractors)
    (gfx-sndtex       . ,gfx-sndtex)
    (gfx-superformula . ,gfx-superformula)
    (gfx-filter       . ,gfx-filter)
    (snd-fft          . ,snd-fft)
    (snd-ifft         . ,snd-ifft)
    (snd-feature      . ,snd-feature)
    (snd-oscillator   . ,snd-oscillator)
    (snd-ringbuffer   . ,snd-ringbuffer)
    (snd-in           . ,snd-in)
    (snd-out          . ,snd-out))))

(define* (hans-create name #:optional args position)
  (hans 'create name (if (eq? args #f) '() args)
                     (if (eq? position #f) '(0 0) position)))

(define (hans-connect source outlet sink inlet)
  (hans 'connect source outlet sink inlet))

(define (hans-modulate src-object src-param src-comp
                       dest-object dest-param dest-comp offset scale)
  (hans 'modulate src-object src-param src-comp
                  dest-object dest-param dest-comp offset scale))
