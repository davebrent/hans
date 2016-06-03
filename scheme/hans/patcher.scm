(define-module (hans patcher)
  :use-module (srfi srfi-9)
  :use-module (srfi srfi-9 gnu)
  :use-module (hans utils)
  :export (make-audio-graph
           make-graphics-graph

           hans-graph
           hans-graph?
           hans-graph-type
           hans-graph-objects
           set-hans-graph-objects!
           hans-graph-connections

           hans-program
           hans-program?
           hans-program-name
           hans-program-audio-graph
           hans-program-graphics-graph

           hans-file
           hans-file?
           hans-file-programs
           hans-file-data
           set-hans-file-data!

           make-environment

           hans-object
           hans-object?
           hans-object-instance-id
           hans-object-rec
           hans-object-name
           hans-object-args
           hans-object-resources
           set-hans-object-resources!
           hans-object-data
           set-hans-object-data!))

(define-record-type <hans-object>
  (hans-object instance-id rec args position)
  hans-object?
  (instance-id hans-object-instance-id)
  (rec         hans-object-rec)
  (args        hans-object-args)
  (position    hans-object-position)
  (resources   hans-object-resources set-hans-object-resources!)
  (data        hans-object-data set-hans-object-data!))

(define-record-type <hans-graph>
  (hans-graph type objects connections)
  hans-graph?
  (type        hans-graph-type)
  (objects     hans-graph-objects set-hans-graph-objects!)
  (connections hans-graph-connections))

(define-record-type <hans-program>
  (hans-program name audio-graph graphics-graph)
  hans-program?
  (name           hans-program-name)
  (audio-graph    hans-program-audio-graph)
  (graphics-graph hans-program-graphics-graph))

(define-record-type <hans-file>
  (hans-file programs)
  hans-file?
  (programs hans-file-programs)
  (data     hans-file-data set-hans-file-data!))

(define (make-audio-graph objects connections)
  (hans-graph 'audio objects connections))

(define (make-graphics-graph objects connections)
  (hans-graph 'graphics objects connections))

(define (make-environment objects)
  "Patching environment for instancing and connecting objects"
  (define ids 0)

  (define (exists? name)
    (not (eqv? (assq-ref objects name) #f)))

  (define (get name)
    (if (exists? name)
      (assq-ref objects name)
      (exit-with-error "Unknown object" name)))

  (define (create name opts position)
    (let ((object (hans-object ids ((get name)) opts position)))
      (set! ids (+ ids 1))
      object))

  (define (connect source outlet sink inlet)
    (if (and (hans-object? source) (hans-object? sink))
      (list (hans-object-instance-id source) outlet
            (hans-object-instance-id sink) inlet)
      (exit-with-error "Not a hans-object" source sink)))

  (let ((instance-id 0))
    (lambda (msg . args)
      (cond ((eqv? msg 'create) (apply create args))
            ((eqv? msg 'connect) (apply connect args))))))
