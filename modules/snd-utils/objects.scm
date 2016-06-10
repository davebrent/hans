(define-module (hans modules snd-utils objects))
(use-modules (hans objects))

(define library "libhans.snd.utils")

(define-public (snd-gain settings args)
  (audio-object "snd-gain"
    library
    "Control audio gain level"
    (list
      (parameter 'amount "Amount of gain to apply" 1 '(1)))))

(define-public (snd-pan settings args)
  (audio-object "snd-pan"
    library
    "Control audio balence"
    (list
      (parameter 'amount "Amount of pan to apply" 1 '(0.5)))))
