(define-module (hans patcher)
  :use-module (srfi srfi-1)
  :use-module (srfi srfi-9)
  :use-module (srfi srfi-9 gnu)
  :use-module (hans utils)
  :export (make-audio-graph
           make-graphics-graph
           make-modulation

           hans-graph
           hans-graph?
           hans-graph-type
           hans-graph-objects
           set-hans-graph-objects!
           hans-graph-connections
           hans-graph-id
           set-hans-graph-id!

           make-program
           hans-program?
           hans-program-name
           hans-program-audio-graph
           hans-program-graphics-graph
           hans-program-modulators

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
           hans-object-position
           hans-object-resources
           set-hans-object-resources!
           hans-object-registers
           set-hans-object-registers!
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
  (registers   hans-object-registers set-hans-object-registers!)
  (data        hans-object-data set-hans-object-data!))

(define-record-type <hans-graph>
  (hans-graph type objects connections)
  hans-graph?
  (type        hans-graph-type)
  (objects     hans-graph-objects set-hans-graph-objects!)
  (connections hans-graph-connections)
  (id          hans-graph-id set-hans-graph-id!))

(define-record-type <hans-program>
  (hans-program name audio-graph graphics-graph modulators)
  hans-program?
  (name           hans-program-name)
  (audio-graph    hans-program-audio-graph)
  (graphics-graph hans-program-graphics-graph)
  (modulators     hans-program-modulators))

(define-record-type <hans-file>
  (hans-file programs)
  hans-file?
  (programs hans-file-programs)
  (data     hans-file-data set-hans-file-data!))

(define (unique-objects connections)
  (delete-duplicates (fold (lambda (conn objects)
                             (append objects (list (list-ref conn 0)
                                                   (list-ref conn 2))))
                           '() connections)
                     (lambda (a b)
                       (eq? (hans-object-instance-id a)
                            (hans-object-instance-id b)))))

(define (make-conns connections)
  (map (lambda (conn)
         (list (hans-object-instance-id (list-ref conn 0))
               (list-ref conn 1)
               (hans-object-instance-id (list-ref conn 2))
               (list-ref conn 3)))
       connections))

(define (make-audio-graph . connections)
  (hans-graph 'audio (unique-objects connections) (make-conns connections)))

(define (make-graphics-graph . connections)
  (hans-graph 'graphics (unique-objects connections) (make-conns connections)))

(define (make-modulation . modulators)
  modulators)

(define* (make-program name #:optional graph-1 graph-2 modulators)
  (define (graph-or-default sym default)
    (if (and (hans-graph? graph-1) (eqv? (hans-graph-type graph-1) sym))
      graph-1
      (if (and (hans-graph? graph-2) (eqv? (hans-graph-type graph-2) sym))
        graph-2
        (default))))
  (hans-program
    name
    (graph-or-default 'audio make-audio-graph)
    (graph-or-default 'graphics make-graphics-graph)
    (if (eq? #t (list? graph-2))
      graph-2
      (if (eq? #t (list? modulators))
        modulators
        '()))))

(define (make-environment settings objects)
  "Patching environment for instancing and connecting objects"
  (define ids 0)

  (define (exists? name)
    (not (eqv? (assq-ref objects name) #f)))

  (define (get name)
    (if (exists? name)
      (assq-ref objects name)
      (exit-with-error "Unknown object" name)))

  (define (create name opts position)
    (let ((object (hans-object ids ((get name) settings opts) opts position)))
      (set! ids (+ ids 1))
      object))

  (define (connect source outlet sink inlet)
    (if (and (hans-object? source) (hans-object? sink))
      (list source outlet sink inlet)
      (exit-with-error "Not a hans-object" source sink)))

  (define (modulate src-object src-param src-component dest-object dest-param
                    dest-component offset scale)
    (if (and (hans-object? src-object) (hans-object? dest-object))
      (list src-object src-param src-component
            dest-object dest-param dest-component
            offset scale)
      (exit-with-error "Not a hans-object" src-object dest-object)))

  (let ((instance-id 0))
    (lambda (msg . args)
      (cond ((eqv? msg 'create) (apply create args))
            ((eqv? msg 'connect) (apply connect args))
            ((eqv? msg 'modulate) (apply modulate args))))))
