(define-module (hans modules snd-io objects))
(use-modules (hans objects))

(define library "libhans.snd.io")

(define-public (snd-in)
  (audio-object "snd-in"
  	library
    "Recieve audio data from a specificed bus"
    '()))

(define-public (snd-out)
  (audio-object "snd-out"
  	library
    "Write data to a specified bus"
    '()))
