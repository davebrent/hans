(define-module (hans seq elektron)
  :use-module (srfi srfi-1)
  :use-module (hans seq)
  :export (md-trigger
           md-ctrl-change))

;; Machine Drum MIDI mappings

(define (md-trigger-lookup track)
  (cond ((eq? 1 track) 36)
        ((eq? 2 track) 38)
        ((eq? 3 track) 40)
        ((eq? 4 track) 41)
        ((eq? 5 track) 43)
        ((eq? 6 track) 45)
        ((eq? 7 track) 47)
        ((eq? 8 track) 48)
        ((eq? 9 track) 50)
        ((eq? 10 track) 52)
        ((eq? 11 track) 53)
        ((eq? 12 track) 55)
        ((eq? 13 track) 57)
        ((eq? 14 track) 59)
        ((eq? 15 track) 60)
        ((eq? 16 track) 62)))

(define* (md-trigger track #:optional (base-channel 0))
  ;; Return a midi message for a machinedrum control trigger event
  (midi-note-on (md-trigger-lookup track) 127 base-channel))

(define-public md-level        'level)
(define-public md-mute         'mute)
(define-public md-param-1      'parameter-1)
(define-public md-param-2      'parameter-2)
(define-public md-param-3      'parameter-3)
(define-public md-param-4      'parameter-4)
(define-public md-param-5      'parameter-4)
(define-public md-param-6      'parameter-4)
(define-public md-param-7      'parameter-4)
(define-public md-param-8      'parameter-4)
(define-public md-am-depth     'am-depth)
(define-public md-am-rate      'am-rate)
(define-public md-eq-freq      'eq-freq)
(define-public md-eq-gain      'eq-gain)
(define-public md-filter-freq  'filter-freq)
(define-public md-filter-width 'filter-width)
(define-public md-filter-q     'filter-q)
(define-public md-samplerate   'samplerate)
(define-public md-distortion   'distortion)
(define-public md-volume       'volume)
(define-public md-pan          'pan)
(define-public md-delay        'delay)
(define-public md-reverb       'reverb)
(define-public md-lfo-speed    'lfo-speed)
(define-public md-lfo-amount   'lfo-amount)
(define-public md-lfo-shape    'lfo-shape)

(define mach-params `(,md-param-1
                      ,md-param-2
                      ,md-param-3
                      ,md-param-4
                      ,md-param-5
                      ,md-param-6
                      ,md-param-7
                      ,md-param-8
                      ,md-am-depth
                      ,md-am-rate
                      ,md-eq-freq
                      ,md-eq-gain
                      ,md-filter-freq
                      ,md-filter-width
                      ,md-filter-q
                      ,md-samplerate
                      ,md-distortion
                      ,md-volume
                      ,md-pan
                      ,md-delay
                      ,md-reverb
                      ,md-lfo-speed
                      ,md-lfo-amount
                      ,md-lfo-shape))

(define-public md-params-all (append `(,md-level ,md-mute) mach-params))

(define (track-ctrl-lookup track)
  (let ((t (modulo (- track 1) 4)))
    (cond ((eq? 0 t) 16) 
          ((eq? 1 t) 40)
          ((eq? 2 t) 72)
          ((eq? 3 t) 96))))

(define (ctrl-lookup track parameter)
  (+ (list-index (lambda (p) (eq? p parameter)) mach-params)
     (track-ctrl-lookup track)))

(define* (md-ctrl-change track parameter value #:optional (base-channel 0))
  ;; Return a midi message for a machinedrum control change event
  (let ((channel (+ 176 base-channel (modulo (- track 1) 4)))
        (controller (if (or (eq? parameter md-level)
                            (eq? parameter md-mute))
                      (+ (modulo track 4) (if (eq? parameter md-level) 7 11))
                      (ctrl-lookup track parameter))))
    (midi-ctrl controller value channel)))
