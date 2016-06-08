(define-module (hans patcher)
  :use-module (srfi srfi-1)
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
           hans-graph-id
           set-hans-graph-id!

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
      (list source outlet sink inlet)
      (exit-with-error "Not a hans-object" source sink)))

  (let ((instance-id 0))
    (lambda (msg . args)
      (cond ((eqv? msg 'create) (apply create args))
            ((eqv? msg 'connect) (apply connect args))))))
