(define-module (hans modules snd-ringbuffer objects))
(use-modules (hans objects))

(define library "libhans.snd.ringbuffer")

(define-public (snd-ringbuffer settings args)
  (audio-object "snd-ringbuffer"
    library
    "Write samples to a ring buffer"
    '()
    '()))
