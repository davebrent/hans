(define-module (hans seq)
  :use-module (srfi srfi-9)
  :use-module (hans extension)
  :use-module (hans utils)
  :export (bpm->ms

           make-midi-out
           midi-out-ports
           midi-out-open
           midi-out-send
           midi-out-close

           midi
           midi?
           midi-status
           midi-byte-1
           midi-byte-2

           midi-note-on
           midi-note-on?
           midi-note-on->off
           midi-note-off
           midi-ctrl

           make-sequencer
           sequencer-track
           sequencer-handler
           sequencer-start
           sequencer-stop
           sequencer-destroy))

;; Tree based pattern generation

(hans-load-extension "libhans.scm.seq" "init_scm_hans_seq")

(define* (bpm->ms beats #:optional pulses)
  (* (if (not pulses) 4 pulses) (/ 60000 beats)))

(define-record-type <midi>
  (midi status byte-1 byte-2)
  midi?
  (status midi-status)
  (byte-1 midi-byte-1)
  (byte-2 midi-byte-2))

(define* (midi-note-on pitch velocity #:optional (channel 0))
  (midi (+ 144 channel) pitch velocity))

(define* (midi-note-off pitch #:optional (channel 0))
  (midi (+ 128 channel) pitch 0))

(define (midi-note-on? event)
  (eq? (logand (midi-status event) 144) 144))

(define (midi-note-on->off event)
  (midi-note-off (midi-byte-1 event) (- (midi-status event) 144)))

(define* (midi-ctrl ctrl value #:optional (channel 0))
  ;; Control change event
  (midi (+ 176 channel) ctrl value))
