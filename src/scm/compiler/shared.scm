(define-module (hans compiler shared)
  :use-module (srfi srfi-1)
  :use-module (hans extension)
  :use-module (hans patcher)
  :use-module (hans objects)
  :export (for-each-graph
           for-each-object
           map-graphs
           map-objects
           list-libraries
           list-objects
           list-graphs
           list-shaders
           list-audio-buffers
           %validate-shaders
           %configure-objects))

(hans-load-extension "libhans.scm.compiler" "scm_init_hans_compiler")

(define (gather lst iterator)
  ;; Gather an iterator function such as for-each into a list of all results
  (let ((result '()))
    (iterator (lambda (res)
                (set! result (append (list res) result))) lst)
    (reverse result)))

(define (for-each-graph proc programs)
  ;; Apply a procedure to each graph
  (for-each (lambda (program)
              (proc (hans-program-audio-graph program))
              (proc (hans-program-graphics-graph program)))
            programs))

(define (for-each-object proc programs)
  ;; Apply a procedure to each object
  (for-each-graph (lambda (graph)
                    (for-each proc (hans-graph-objects graph)))
                  programs))

(define (map-graphs proc programs)
  ;; Map a procedure over all graphs
  (map proc (gather programs for-each-graph)))

(define (map-objects proc programs)
  ;; Map a procedure over all objects
  (map proc (gather programs for-each-object)))

(define (list-libraries programs)
  ;; Returns all libraries
  (delete-duplicates (map-objects (lambda (object)
                       (object-record-library (hans-object-rec object)))
                     programs)))

(define (list-objects programs)
  ;; Returns all objects
  (delete-duplicates (gather programs for-each-object)))

(define (list-graphs programs)
  ;; Returns all graphs
  (gather programs for-each-graph))

(define (list-shaders programs)
  ;; Returns all shaders
  (fold append '() (map (compose object-record-shaders hans-object-rec)
                        (list-objects programs))))

(define (list-audio-buffers programs)
  ;; Returns all audio buffers
  (fold append '() (map (compose object-record-audio-buffers hans-object-rec)
                        (list-objects programs))))
