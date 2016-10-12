(define-module (hans connect elektron)
  :use-module (srfi srfi-1)
  :export (md-ctrl-change))

;; Machine Drum MIDI mappings

(define-public (md-trigger track)
  ;; Return midi status byte for a trigger event
  (cond ((eq? 1) 36)  ((eq? 2) 38)  ((eq? 3) 40)  ((eq? 4) 41)
        ((eq? 5) 43)  ((eq? 6) 45)  ((eq? 7) 47)  ((eq? 8) 48)
        ((eq? 9) 50)  ((eq? 10) 52) ((eq? 11) 53) ((eq? 12) 55)
        ((eq? 13) 57) ((eq? 14) 59) ((eq? 15) 60) ((eq? 16) 62)))

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
  (let ((first-byte (+ 176 base-channel (modulo (- track 1) 4)))
        (second-byte (if (or (eq? parameter md-level)
                             (eq? parameter md-mute))
                       (+ (modulo track 4) (if (eq? parameter md-level) 7 11))
                       (ctrl-lookup track parameter))))
    `(,first-byte ,second-byte ,value)))
