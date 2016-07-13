;; Patterns are functions that take no arguments and return values on each call.
;; A pattern is determined to have finished when it returns stop-iteration.

(define-module (hans patterns)
  :export (pattern-stop
           pattern-stopped?
           pattern->list
           make-pseq
           make-prange
           make-prand
           make-pconcat))

(define (pattern-stop)
  "Raises a signal that a pattern should be stopped"
  'stop-iteration)

(define (pattern-stopped? value)
  "Returns true if the pattern has stopped"
  (eq? value 'stop-iteration))

(define (pattern->list pattern)
  "Returns the list of values from calling a pattern until it stops"
  (define (perform lst)
    (let ((value (pattern)))
      (cond ((pattern-stopped? value) lst)
            (else (perform (append lst (list value)))))))
  (perform '()))

(define (make-pseq lst repeats)
  "Returns values in a list one after another"
  (let ((len (length lst))
        (idx 0)
        (iterations 0))
    (lambda ()
      (cond ((>= iterations repeats) (pattern-stop))
            (else (begin
              (let ((val (list-ref lst idx)))
                (set! idx (modulo (+ 1 idx) len))
                (if (= idx 0) (set! iterations (+ iterations 1)))
                val)))))))

(define (make-prange limit repeats)
  "Returns a sequence of an arithmetic progression of integers"
  (make-pseq (iota limit) repeats))

(define (make-prand lst)
  "Choose items from the list randomly"
  (let ((state (seed->random-state (current-time)))
        (len (length lst)))
    (lambda ()
      (list-ref lst (random len state)))))

(define (make-pshuff lst repeats)
  "Shuffle the list in random order and use the same random order N times"
  (define (shuffle x)
    (do ((v (list->vector x)) (n (length x) (- n 1)))
        ((zero? n) (vector->list v))
      (let* ((r (random n)) (t (vector-ref v r)))
        (vector-set! v r (vector-ref v (- n 1)))
        (vector-set! v (- n 1) t))))
  (make-pseq (shuffle lst) repeats))

(define (make-pconcat pattern-1 pattern-2 repeats)
  "Play one pattern after the other"
  (make-pseq (append (pattern->list pattern-1)
                     (pattern->list pattern-2)) repeats))
