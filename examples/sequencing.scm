(use-modules (srfi srfi-1)
             (hans connect)
             (hans sequencer)
             (examples patterns))

(define (midi-handler dev)
  (lambda (track value onset?)
    (if onset?
      (midi-out-send dev 144 value 127)
      (midi-out-send dev 128 value 0))))

(define (play-pattern the-pattern)
  (let ((dev (make-midi-out))
        (seq (make-sequencer)))

    ;; Add each pattern as a track to the sequencer
    (for-each (lambda (patt)
                (let ((the-sequence (pattern->sequence (last patt))))
                  (sequencer-track seq (car patt) the-sequence)))
              the-pattern)

    ;; Set the sequence event handler and start
    (sequencer-handler seq (midi-handler dev))
    (midi-out-open dev 0)
    (sequencer-start seq)

    ;; Return a procedure to stop & destroy the sequencer
    (lambda ()
      (sequencer-stop seq)
      (midi-out-close dev)
      (sequencer-destroy seq))))

(define stop-pattern (play-pattern (patt1 120)))
(sleep 20)
(stop-pattern)
