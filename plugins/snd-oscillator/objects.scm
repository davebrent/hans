(define-module (hans plugin snd-oscillator objects)
  :use-module (hans objects))

(define-public (snd-oscillator settings args)
  (audio-object "snd-oscillator"
    "libhans.snd.oscillator"
    "An audio oscillator supporting multiple waveforms"
    `(,(parameter 'frequency 440 "Oscillator frequency")
      ,(parameter 'waveform 1 "Oscillator waveform"))
    (list (audio-buffer "snd/osc/buffer" 1 (assq-ref settings 'blocksize)))))
