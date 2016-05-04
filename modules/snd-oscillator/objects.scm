(define-module (hans modules snd-oscillator objects))
(use-modules (hans objects))

(define library "libhans.snd.oscillator")

(define-public (snd-oscillator)
  (audio-object "snd-oscillator"
  	library
    "An audio oscillator supporting multiple waveforms"
    (list
      (parameter 'frequency "The frequency of the oscillator" 1 '(440))
      (parameter 'waveform "The waveform to play" 1 '(0)))))
