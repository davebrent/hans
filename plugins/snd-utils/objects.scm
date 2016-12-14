(define-module (hans plugin snd-utils objects)
  :use-module (hans objects))

(define library "libhans.snd.utils")

(define-public (snd-gain settings args)
  (audio-object "snd-gain"
    library
    "Control audio gain level"
    `(,(parameter 'amount 1 "Amount of gain to apply"))))

(define-public (snd-pan settings args)
  (audio-object "snd-pan"
    library
    "Control audio balence"
    `(,(parameter 'amount 0.5 "Amount of pan to apply"))))
