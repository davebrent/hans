(define-module (hans plugin snd-ringbuffer objects)
  :use-module (hans objects))

(define-public (snd-ringbuffer settings args)
  (audio-object "snd-ringbuffer"
    "libhans.snd.ringbuffer"
    "Write samples to a ring buffer"
    '()
    '()))
