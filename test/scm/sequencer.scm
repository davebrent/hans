(define-module (hans test sequencer)
  #:use-module (srfi srfi-64)
  #:use-module (hans sequencer))

(test-begin "test-sequencer")

(test-equal 23.0 (round (get-tick-size 110 4 4 24)))
(test-equal 42.0 (round (get-tick-size 60 4 4 24)))
(test-equal 12.0 (round (get-tick-size 200 4 4 24)))
