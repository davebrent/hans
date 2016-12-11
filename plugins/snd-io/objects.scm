(define-module (hans plugin snd-io objects)
  :use-module (hans objects))

(define library "libhans.snd.io")

(define-public (snd-in settings args)
  (audio-object "snd-in"
    library
    "Read data from a specificed bus"
    '()
    (list (audio-buffer "snd/in/buffer"
                        (assq-ref settings 'channels)
                        (assq-ref settings 'blocksize)))))

(define-public (snd-out settings args)
  (audio-object "snd-out"
    library
    "Write data to a specified bus"
    '()
    (list (audio-buffer "snd/out/buffer"
                        (assq-ref settings 'channels)
                        (assq-ref settings 'blocksize)))))
