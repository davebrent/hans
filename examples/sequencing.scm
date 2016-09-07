(use-modules (hans sequencer)
             (hans control))

(define dev (make-midi-out))
(define seq (make-sequencer))
(define kick (pattern 72 ~ ~ ~ 60 ~ ~ ~ 60 ~ ~ ~ 60 ~ ~ ~))

(sequencer-track seq (bpm->ms 120 4) (pattern->sequence kick))
(sequencer-handler seq (lambda (value onset?)
                         (if onset?
                           (midi-out-send dev 144 value 127)
                           (midi-out-send dev 128 value 0))))

(midi-out-open dev 0)
(sequencer-start seq)

(sequencer-stop seq)
(midi-out-close dev)
(sequencer-destroy seq)
