(define-module (hans sequencer)
  :export (flush
           make-transport
           set-pulse!
           set-beat!
           set-bar!
           get-pulse
           get-beat
           get-bar
           make-note
           make-ctrl
           make-time
           make-metro-scheduler
           make-musical-scheduler
           make-midi-out-device
           make-console-device
           get-tick-size
           schedule
           send-event
           run))

(load-extension "libhans-sequencer-bindings" "scm_init_sequencer_module")

;; Transport

(define (make-transport pulses-per-beat)
  ;; Return data for the transport
  (list 'time pulses-per-beat 0 0 0))

(define (set-pulse! transport pulse)
  (list-set! transport 2 pulse))

(define (set-beat! transport beat)
  (list-set! transport 3 beat))

(define (set-bar! transport bar)
  (list-set! transport 4 bar))

(define (get-pulse transport)
  (list-ref transport 2))

(define (get-beat transport)
  (list-ref transport 3))

(define (get-bar transport)
  (list-ref transport 4))

;; Event functions

(define (make-note pitch velocity duration channel)
  ;; Return data for a note
  (list 'note pitch velocity duration channel))

(define (make-ctrl controller value target duration channel)
  ;; Return data for a control event
  (list 'ctrl controller value target duration channel))

(define (make-time status)
  ;; Return data for a time event
  (list 'clock status))

;; Sequencers

(define (make-metro-scheduler interval)
  ;; A simple schedular that has no concept of bars and beats
  (let ((transport (make-transport 0)))
    (make-scheduler interval
      (lambda (delta generators device)
        (let ((generators
                (filter (lambda (generator) (generator device transport))
                        generators)))
          (send-event device transport)
          (set-pulse! transport (+ 1 (get-pulse transport)))
          generators)))))

(define (get-tick-size bpm beats-in-bar beat-unit tpb)
  ;; Returns the size in milliseconds of a midi tick
  ;; beat-unit    -- The note value that represents one beat
  ;; beats-in-bar -- How many such beats are grouped to make a bar
  ;; tpb          -- How many ticks there are between each beat
  (rationalize (/ (/ (* beats-in-bar (/ 60000 bpm)) beat-unit) tpb) 0.0000001))

(define (make-musical-scheduler bpm beats-in-bar beat-unit)
  ;; Returns a scheduler using a more musical notation
  (let* ((ticks-per-beat 24)
         (tick-size (get-tick-size bpm beats-in-bar beat-unit ticks-per-beat))
         (transport (make-transport ticks-per-beat)))

    (make-scheduler tick-size
      (lambda (delta generators device)
        (let ((pulse (get-pulse transport))
              (generators
                (filter (lambda (generator) (generator device transport))
                        generators)))
          (send-event device transport)

          (if (and (= pulse 0) (= (get-bar transport) 0))
            (send-event device (make-time 'start)))

          (send-event device (make-time 'tick))
          (set-pulse! transport (+ 1 (get-pulse transport)))

          (if (= pulse (- ticks-per-beat 1))
            (begin
              (set-pulse! transport 0)
              (set-beat! transport (+ 1 (get-beat transport)))
              (if (= (get-beat transport) beats-in-bar)
                (begin
                  (set-beat! transport 0)
                  (set-bar! transport (+ 1 (get-bar transport)))))))

          generators)))))
