(define-module (hans plugin snd-oscillator objects)
  :use-module (hans objects))

(define-public (snd-oscillator settings args)
  (audio-object "snd-oscillator"
    "libhans.snd.oscillator"
    "An audio oscillator supporting multiple waveforms"
    (list
      (parameter 'frequency "The frequency of the oscillator" 1 '(440))
      (parameter 'waveform "The waveform to play" 1 '(0)))
    (list (audio-buffer "snd/osc/buffer" 1 (assq-ref settings 'blocksize)))))
