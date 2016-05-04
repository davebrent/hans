(define-module (hans modules snd-loadfile objects))
(use-modules (hans objects))

(define library "libhans.snd.loadfile")

(define-public (snd-loadfile)
  (audio-object "snd-loadfile"
  	library
    "Loads a sound file into a named audio buffer"
    '()))
