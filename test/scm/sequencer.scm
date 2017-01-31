(define-module (hans test sequencer)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-64)
  #:use-module (hans sequencer)
  #:use-module (hans utils))

(test-begin "test-sequencer")

; (define (sort-events events)
;   (sort events (lambda (a b) (- (car a) (car b)))))
; 
; (define (pattern-events patt dur)
;   (sort-events ((pattern->sequence patt) (make-cycle 0 dur 0))))
; 
; ;; Test node predicates
; (begin
;   (test-equal (group-node? (group-node)) #t)
;   (test-equal (value-node? (value-node 1)) #t)
;   (test-equal (rest-node? (rest-node)) #t)
;   (test-equal (function-node? (function-node (lambda () #t))) #t)
;   (test-equal (group-node? (group-node)) #t)
;   (test-equal (group-node? #f) #f)
;   (test-equal (value-node? #f) #f)
;   (test-equal (rest-node? #f) #f)
;   (test-equal (function-node? #f) #f)
;   (test-equal (group-node? #f) #f))
; 
; ;; Simple patterns
; (let ((patt (pattern 1 ~ 2 ~)))
;   (test-equal `((0 250 1) (500 250 2))
;               (pattern-events patt 1000)))
; 
; ;; Nested patterns
; (let ((patt (pattern 1 ~ (2 3) ~)))
;   (test-equal `((0 250 1) (500 125 2) (625 125 3))
;               (pattern-events patt 1000)))
; 
; ;; Repeating a pattern
; (let ((patt (pattern ,(pattern-repeat (pattern 1) 2))))
;   (test-equal `((0 500 1) (500 500 1))
;               (pattern-events patt 1000)))
; 
; ;; Reversing a pattern
; (let ((patt (pattern ,(pattern-reverse (pattern 1 ~ 2 ~)))))
;   (test-equal `((250 250 2) (750 250 1))
;               (pattern-events patt 1000)))
; 
; ;; Degrading a pattern may return an event OR may not?
; (let ((patt (pattern ,(pattern-degrade (pattern 1 1 1 1) 50))))
;   (pattern-events patt 1000)
;   (test-equal #t #t))
; 
; ;; Shuffling a pattern, probably shouldnt return the same pattern?
; (let ((patt (pattern ,(pattern-shuffle (pattern 1 2 3 4) 50))))
;   (test-equal (not (eq? '(1 2 3 4)
;                         (map last (pattern-events patt 1000))))
;               #t))
; 
; ;; Rotating a pattern right
; (let ((patt (pattern ,(pattern-rotate (pattern 1 ~ 2 ~) 1))))
;   (test-equal `((250 250 1) (750 250 2))
;               (pattern-events patt 1000)))
; 
; ;; Rotating a pattern left
; (let ((patt (pattern ,(pattern-rotate (pattern 1 ~ 2 ~) -1))))
;   (test-equal `((250 250 2) (750 250 1))
;               (pattern-events patt 1000)))
