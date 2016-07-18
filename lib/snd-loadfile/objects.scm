(define-module (hans lib snd-loadfile objects))
(use-modules (hans objects))

(define library "libhans.snd.loadfile")

(define-public (snd-loadfile settings args)
  (audio-object "snd-loadfile"
    library
    "Loads a sound file into a named audio buffer"
    '()))
