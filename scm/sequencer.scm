(define-module (hans sequencer)
  :use-module (srfi srfi-1)
  :use-module (srfi srfi-9)
  :use-module (srfi srfi-9 gnu)
  :use-module (hans extension)
  :use-module (hans utils)
  :export (bpm->ms

           make-cycle
           cycle-start
           cycle-duration
           cycle-number

           make-sequencer
           sequencer-track
           sequencer-handler
           sequencer-start
           sequencer-stop
           sequencer-destroy

           group-node
           group-node?
           value-node
           value-node?
           rest-node
           rest-node?
           function-node
           function-node?
           pnode-length

           tracks-dump

           pattern
           make-pattern
           pattern-transform
           pattern-eval
           pattern-resolve
           pattern->sequence

           pattern-repeat
           pattern-reverse
           pattern-degrade
           pattern-shuffle
           pattern-rotate))

;; Tree based pattern generation

(hans-load-extension "libhanssequencer" "scm_init_sequencer_module")

(define* (bpm->ms beats #:optional pulses)
  (* (if (not pulses) 4 pulses) (/ 60000 beats)))

(define-record-type <pnode>
  (pnode type value)
  pnode?
  (type pnode-type)
  (value pnode-value set-pnode-value!)
  (start pnode-start set-pnode-start!)
  (duration pnode-duration set-pnode-duration!))

(set-record-type-printer! <pnode>
  (lambda (record port)
    (write-char #\[ port)
    (display (pnode-type record))
    (display " ")
    (display (pnode-value record))
    (display " ")
    (display (pnode-start record))
    (display " ")
    (display (pnode-duration record))
    (write-char #\] port)))

(define GROUP 'group)
(define FUNCTION 'function)
(define VALUE 'value)
(define REST 'rest)

(define (group-node . children) (pnode GROUP children))
(define (value-node value) (pnode VALUE value))
(define (function-node procedure) (pnode FUNCTION procedure))
(define (rest-node) (pnode REST #f))

(define (make-pnode-predicate type)
  (lambda (node)
    (eq? (and (pnode? node)
              (pnode-type node)) type)))

(define group-node? (make-pnode-predicate GROUP))
(define value-node? (make-pnode-predicate VALUE))
(define rest-node? (make-pnode-predicate REST))
(define function-node? (make-pnode-predicate FUNCTION))

(define (pnode-copy node)
  (let ((copied (pnode (pnode-type node) (pnode-value node))))
    (set-pnode-start! node (pnode-start node))
    (set-pnode-duration! node (pnode-duration node))
    ;; Copy and set children if its a group node
    (if (group-node? copied)
      (set-pnode-value! copied (map pnode-copy (pnode-value copied))))
    copied))

(define (set-pnode-cycle! node cycle)
  ;; Set time of a pnode
  (set-pnode-start! node (cycle-start cycle))
  (set-pnode-duration! node (cycle-duration cycle)))

(define (pnode-length node)
  ;; Return the number of child nodes
  (if (group-node? node)
    (length (pnode-value node))
    1))

(define (pnode-add-child! parent child)
  ;; Add a child to a parent node, returning the parent
  (set-pnode-value! parent (append (pnode-value parent) `(,child)))
  parent)

(define (pnode-add-child parent child)
  ;; Add a child to a parent node, returning the child
  (pnode-add-child! parent child)
  child)

(define (token-rest? tk) (eq? tk '~))
(define (token-group? tk) (list? tk))
(define (token-function? tk) (procedure? tk))
(define (token-value? tk)
  (and (not (token-rest? tk))
       (not (token-group? tk))
       (not (token-function? tk))))

(define (make-pattern args root)
  ;; Create a pattern tree
  (cond ((null? args)
          root)
        ((null? root)
          (make-pattern args (group-node)))
        ((token-function? (car args))
          (pnode-add-child! root (function-node (car args)))
          (make-pattern (cdr args) root))
        ((token-group? (car args))
          (begin
            (pnode-add-child! root (make-pattern (car args) (group-node)))
            (make-pattern (cdr args) root)))
        ((token-rest? (car args))
          (begin
            (pnode-add-child! root (rest-node))
            (make-pattern (cdr args) root)))
        ((token-value? (car args))
          (begin
            (pnode-add-child! root (value-node (car args)))
            (make-pattern (cdr args) root)))
        (else
          (print "Unknown node type" (car args)))))

(define (pattern-eval output input cycle)
  ;; Evaluate a pattern with a cycle, returning a simple tree
  (cond ((or (value-node? input)
             (rest-node? input))
          (set-pnode-cycle! input cycle)
          (pnode-add-child output input))
        ;; Evaluate function and traverse children
        ((function-node? input)
          (pattern-eval (pnode-add-child output (group-node))
                        ((pnode-value input) cycle)
                        cycle))
        ;; Split the cycle and traverse children
        ((group-node? input)
          (let* ((len (pnode-length input))
                 (duration (/ (cycle-duration cycle) (if (eq? 0 len) 1 len)))
                 (index 0))
            (for-each (lambda (node)
                        (let ((cycle (make-cycle (+ (* index duration)
                                                    (cycle-start cycle))
                                                 duration
                                                 (cycle-number cycle))))
                          (pattern-eval output node cycle)
                          (set! index (+ 1 index))))
                      (pnode-value input))))))

(define (pattern->sequence . patterns)
  ;; Returns a procedure, that when evaluated returns a list of pattern events.
  (define (traverse tree visitor)
    (cond ((group-node? tree)
           (for-each (lambda (node)
                       (traverse node visitor))
                     (pnode-value tree)))
          ((value-node? tree)
           (visitor tree))))

  (lambda (cycle)
    (let ((events '()))
      (for-each (lambda (patt)
                  (let ((dest (group-node)))
                    (pattern-eval dest patt cycle)
                    (traverse dest (lambda (node)
                                     (set! events
                                       (append `((,(pnode-start node)
                                                  ,(pnode-duration node)
                                                  ,(pnode-value node)))
                                               events))))))
                patterns)
      events)))

(define (tracks-dump tracks)
  ;; Dump events from all tracks to complete an entire loop
  (let ((the-lcm (apply lcm (map car tracks))))
    (map-in-order (lambda (track)
                    (let ((dur (car track))
                          (seq (pattern->sequence (last track)))
                          (events '()))
                      (for-n (/ the-lcm dur) (lambda (i)
                        (let ((cycle (make-cycle 0 dur i)))
                          (set! events
                            (append events `(,
                              (cons cycle (reverse (seq cycle)))))))))
                      events))
                  tracks)))

(define-syntax pattern
  ;; Short hand for creating a pattern tree
  (syntax-rules ()
    ((pattern args ...)
      (make-pattern `(args ...) '()))))

(define-syntax pattern-transform
  ;; Return a function that checks and re-evaluates returned nodes
  (syntax-rules ()
    ((pattern-transform name args context body ...)
      (define name (lambda args
                     (lambda (cycle)
                       (let ((dest (group-node))
                             (next ((lambda context body ...) cycle)))
                         (pattern-eval dest next cycle)
                         dest)))))))

(define (pattern-resolve node cycle)
  ;; Transforms may be chained together so a transform may recieve a procedure
  ;; instead of a node as an argument
  (if (pnode? node)
    node
    (pattern-resolve (node cycle) cycle)))

(pattern-transform pattern-repeat (patt n) (cycle)
  ;; Repeat a pattern N times
  (let ((node (pattern-resolve patt cycle)))
    (apply group-node (map (lambda (_)
                             (pnode-copy node))
                           (make-list n)))))

(pattern-transform pattern-reverse (patt) (cycle)
  ;; Reverse the order of a pattern
  (let ((src (pattern-resolve patt cycle)))
    (if (group-node? src)
      (apply group-node (reverse (pnode-value src)))
      src)))

(pattern-transform pattern-degrade (patt pc) (cycle)
  ;; Randomly skip nodes in a pattern
  (let ((src (pattern-resolve patt cycle)))
    (apply group-node (partition (lambda (node)
                                   (< (random 100) pc))
                                 (if (group-node? src)
                                   (pnode-value src)
                                   `(,src))))))

(pattern-transform pattern-shuffle (patt pc) (cycle)
  ;; Shuffle a pattern
  (let ((src (pattern-resolve patt cycle)))
    (if (group-node? src)
      (apply group-node (shuffle (pnode-value src)))
      src)))

(pattern-transform pattern-rotate (patt n) (cycle)
  ;; Rotate pattern every cycle by N
  ;; `N` may be a procedure or a number (positive right, negative left)
  (let ((src (pattern-resolve patt cycle)))
    (if (group-node? src)
      (let* ((amount (if (procedure? n) (n cycle) n))
             (procedure (if (> amount 0) rotate-right rotate-left)))
        (apply group-node (procedure (pnode-value src) (abs amount))))
      src)))
