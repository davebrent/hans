(define-module (examples common)
  :use-module (hans patcher)
  :use-module (hans lib snd-io objects)
  :use-module (hans lib snd-oscillator objects)
  :use-module (hans lib snd-ringbuffer objects)
  :use-module (hans lib gfx-attractors objects)
  :use-module (hans lib gfx-superformula objects)
  :use-module (hans lib gfx-quad objects)
  :use-module (hans lib gfx-scopes objects)
  :use-module (hans lib gfx-script objects)
  :use-module (hans lib gfx-sndtex objects)
  :use-module (hans lib gfx-filter objects)
  :export (settings
           hans-create
           hans-connect
           hans-modulate))

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
    (gfx-attractors   . ,gfx-attractors)
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

(define (hans-modulate src-object src-param src-comp
                       dest-object dest-param dest-comp offset scale)
  (hans 'modulate src-object src-param src-comp
                  dest-object dest-param dest-comp offset scale))
