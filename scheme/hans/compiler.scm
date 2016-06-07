(define-module (hans compiler)
  :use-module (srfi srfi-1)
  :use-module (hans utils)
  :use-module (hans os)
  :use-module (hans patcher)
  :use-module (hans objects)
  :export (hans-compile

           for-each-graph
           for-each-object
           map-graphs
           map-objects
           list-libraries
           list-objects
           list-graphs

           topological-sort-pass
           resolve-library-path-pass
           object-resources-pass
           validate-shader-pass
           validate-connections-pass
           create-requested-resources-pass))

(load-extension "libhans-compiler-bindings" "scm_init_hans_compiler_module")

(define (gather lst iterator)
  "Gather an iterator function such as for-each into a list of all results"
  (let ((result '()))
    (iterator (lambda (res)
                (set! result (append (list res) result))) lst)
    (reverse result)))

(define (for-each-graph proc file)
  "Apply a procedure to each graph in a hans file"
  (for-each (lambda (program)
              (proc (hans-program-audio-graph program))
              (proc (hans-program-graphics-graph program)))
            (hans-file-programs file)))

(define (for-each-object proc file)
  "Apply a procedure to each object in a hans file"
  (for-each-graph (lambda (graph)
                    (for-each proc (hans-graph-objects graph)))
                  file))

(define (map-graphs proc file)
  "Map a procedure over all objects in a hans file"
  (map proc (gather file for-each-graph)))

(define (map-objects proc file)
  "Map a procedure over all objects in a hans file"
  (map proc (gather file for-each-object)))

(define (list-libraries file)
  "Returns all libraries used in a hans file"
  (delete-duplicates (map-objects (lambda (object)
                       (object-record-library (hans-object-rec object)))
                     file)))

(define (list-objects file)
  "Returns all objects used in a hans file"
  (delete-duplicates (gather file for-each-object)))

(define (list-graphs file)
  "Returns all graphs used in a hans file"
  (gather file for-each-graph))

(define (list-shaders file)
  "Returns all shaders used in a hans file"
  (fold append '() (map (compose object-record-shaders hans-object-rec)
                        (list-objects file))))

(define (resolve-library-path-pass file options)
  "Compiler pass that sets the full objects library extension path"

  (define (get-search-paths options)
    "Create a list of library search paths"
    (let ((from-options (assq-ref options 'library-paths))
          (from-env (string-split (os-getenv "HANS_LIBRARY_PATH" "") #\:)))
      (if (eq? from-options #f)
        from-env
        (append from-options from-env))))

  (define (resolve-library name paths)
    "Returns the location of a library or throws an error if not found"
    ;; TODO: Swap shared library extension based on platform

    (let* ((library (string-append name ".dylib"))
           (possible (map (lambda (path)
                            (os-path-join path library)) paths))
           (result (filter file-exists? possible)))
      (if (equal? result '())
        (exit-with-error "Unable to find library -" library "in" possible)
        (car result))))

  (let* ((search-paths (get-search-paths options))
         (library-paths (map (lambda (name)
                               (cons name (resolve-library name search-paths)))
                             (list-libraries file))))
    (for-each-object (lambda (object)
      (let* ((rec (hans-object-rec object))
             (lib (assq-ref library-paths (object-record-library rec))))
        (if (not (eq? #f lib))
          (set-object-record-library! rec lib))))
      file)
    file))

(define (topological-sort-pass file options)
  "Topologicaly sort graphs contained in a hans file"

  (define (graph->adjacency-list graph)
    "Transform a hans call graph into a simpler adjacency list"
    ;; TODO: Remove or just use an adjacency list through out
    (define (simplify-connections connections)
      (delete-duplicates
        (map (lambda (conn)
          (cons (list-ref conn 0)
                (list-ref conn 2))) connections)))

    (define (object-connections id connections)
      (delete-duplicates
        (map cdr (filter (lambda (conn) (eqv? (car conn) id)) connections))))

    (let ((conns (simplify-connections (hans-graph-connections graph))))
      (map (lambda (object)
             (let ((id (hans-object-instance-id object)))
               (append (list id) (object-connections id conns))))
           (hans-graph-objects graph))))

  (define (topological-sort-helper dag insert lookup)
    ;; Taken from tsort.scm from SLIB
    (if (null? dag)
      '()
      (let* ((adj-table (make-hash-table)) (sorted '()))
        (letrec ((visit (lambda (u adj-list)
                          ;; Color vertex u
                          (insert adj-table u 'colored)
                          ;; Visit uncolored vertices which u connects to
                          (for-each (lambda (v)
                                      (let ((val (lookup adj-table v)))
                                        (if (not (eq? val 'colored))
                                            (visit v (or val '())))))
                                    adj-list)
                          ;; Since all vertices downstream u are visited
                          ;; by now, we can safely put u on the output list
                          (set! sorted (cons u sorted)))))
          ;; Hash adjacency lists
          (for-each (lambda (def)
                (insert adj-table (car def) (cdr def)))
              (cdr dag))
          ;; Visit vertices
          (visit (caar dag) (cdar dag))
          (for-each (lambda (def)
                (let ((val (lookup adj-table (car def))))
            (if (not (eq? val 'colored))
                (visit (car def) (cdr def)))))
              (cdr dag)))
        sorted)))

  (define (sort-graph graph)
    (let ((sorted (topological-sort-helper
                  (graph->adjacency-list graph)
                  hash-set! hash-ref))
          (objects (hans-graph-objects graph)))
      (set-hans-graph-objects!
        graph
        (map (lambda (id)
               (find (lambda (obj)
                       (equal? (hans-object-instance-id obj) id))
                     objects))
             sorted))))

  (for-each-graph sort-graph file)
  file)

(define (validate-shader-pass file options)
  "Validate all shaders in a hans file"
  (for-each-object (lambda (obj)
    (let ((rec (hans-object-rec obj)))
      (if (graphics-object? rec)
        (for-each (lambda (shader)
          (if (not (valid-shader? (shader-type shader) (shader-code shader)))
            (exit-with-error "Invalid shader" (shader-name shader)
                                              (object-record-name rec))))
          (object-record-shaders rec)))))
    file)
  file)

(define (object-resources-pass file options)
  "Fill in object runtime resource requests"
  (let* ((the-libraries  (list-libraries file))
         (the-objects    (list-objects file))
         (object-records (map hans-object-rec the-objects))
         (object-alists  (map record->alist object-records)))
    (for-each (lambda (obj-res)
        (set-hans-object-data! (car obj-res) (car (last obj-res)))
        (set-hans-object-resources! (car obj-res) (last (last obj-res))))
      (zip the-objects
           (get-object-info
              the-libraries
              (map (lambda (data)
                     (acons 'args (hans-object-args (last data)) (car data)))
                   (zip object-alists the-objects))))))
  file)

(define (validate-connections-pass file options)
  "Check that connections match the requested number of inlets & outlets"

  (define (or-zero value)
    (if (equal? #f value) -1 value))

  (define (valid-connections? id conns num-inlets num-outlets)
    (define result #t)
    (for-each (lambda (conn)
      (let ((source (list-ref conn 0))
            (outlet (list-ref conn 1))
            (sink   (list-ref conn 2))
            (inlet  (list-ref conn 3)))
        (if (equal? source id)
          (if (>= outlet num-outlets)
            (set! result "bad outlet."))
          (if (equal? sink id)
            (if (>= inlet num-inlets)
              (set! result "bad inlet."))))))
      conns)
    result)

  (for-each-graph (lambda (graph)
    (let ((objects (hans-graph-objects graph))
          (connections (hans-graph-connections graph)))
      (for-each (lambda (obj)
        (let* ((resources (hans-object-resources obj))
               (num-inlets (assoc-ref resources 'inlet))
               (num-outlets (assoc-ref resources 'outlet))
               (result (valid-connections? (hans-object-instance-id obj)
                                           connections
                                           (or-zero num-inlets)
                                           (or-zero num-outlets))))
          (if (not (equal? result #t))
            (exit-with-error "Invalid connection,"
                             "["
                             (hans-object-instance-id obj)
                             (object-record-name (hans-object-rec obj))
                              "]"
                             result
                             resources))))
        objects)))
    file)
  file)

(define (create-requested-resources-pass file options)
  "Creates a blob of all the requested resources"

  (define (do-libraries writer file)
    (write-libraries writer (list-libraries file)))

  (define (do-objects writer file)
    (write-objects
      writer
      (map (lambda (obj)
             (let ((rec (hans-object-rec obj))
                   (instance-id (hans-object-instance-id obj))
                   (item '()))
               (set! item (acons 'instance-id instance-id item))
               (set! item (acons 'name (object-record-name rec) item))
               (set! item (acons 'type (object-record-type rec) item))
               item))
           (list-objects file))))

  (define (do-parameters writer file)
    (define the-values '())

    (define (process-param obj param)
      (let ((offset (length the-values)))
        (set! the-values (append the-values (parameter-value param)))
        `((instance-id . ,(hans-object-instance-id obj))
          (name        . ,(symbol->string (parameter-name param)))
          (size        . ,(parameter-components param))
          (value       . offset))))

    (let ((items (fold (lambda (obj parameters)
                         (append parameters
                                 (map (lambda (param) (process-param obj param))
                                      (object-record-parameters
                                        (hans-object-rec obj)))))
                       '() (list-objects file))))
      (+ (write-parameters writer items)
         (write-parameter-values writer the-values))))

  (define (do-programs writer file)
    (define the-programs '())
    (define the-graphs '())
    (define the-connections '())

    (define (push-graph graph)
      (let* ((start (length the-graphs))
             (data (map hans-object-instance-id (hans-graph-objects graph)))
             (start-edges (length the-connections))
             (edges (hans-graph-connections graph))
             (end-edges (+ start-edges (length edges)))
             (end (+ start (length data))))
        (if (or (eq? (length data) 0) (eq? (length edges) 0)) '()
          (begin
            (set! the-graphs (append the-graphs data))
            (set! the-connections (append the-connections edges))
            (list start end start-edges end-edges)))))

    (for-each (lambda (pgm)
        (set! the-programs (append the-programs (list
          `((name     . ,(hans-program-name pgm))
            (audio    . ,(push-graph (hans-program-audio-graph pgm)))
            (graphics . ,(push-graph (hans-program-graphics-graph pgm))))))))
      (hans-file-programs file))

    (+ (write-programs writer the-programs (length the-graphs)
                                           (length the-connections))
       (write-graphs writer the-graphs)
       (write-graph-connections writer the-connections)))

  (define (do-fbos writer file)
    (define the-fbos '())
    (define the-attachments '())
    (define (process-obj obj)
      (let ((rec (hans-object-rec obj)))
        (if (graphics-object? rec)
          (let* ((fbo         (object-record-fbo rec))
                 (start       (length the-attachments))
                 (attachments (fbo-attachments fbo)))
            (set! the-attachments (append the-attachments attachments))
            (set! the-fbos (append the-fbos (list
              `((instance-id    . ,(hans-object-instance-id obj))
                (stencil-buffer . ,(fbo-stencil-buffer fbo))
                (attachments    . ,(list
                  start (+ start (length attachments))))))))))))

    (for-each-object process-obj file)

    (+ (write-fbos writer the-fbos)
       (write-fbo-attachments writer (map record->alist the-attachments))))

  (define (do-shaders writer file)
    (write-shaders writer (map record->alist (list-shaders file))))

  (define (do-strings writer file)
    (define the-strings '())
    (set! the-strings (append the-strings (list-libraries file)))
    (set! the-strings (append the-strings (map (compose object-record-name
                                                        hans-object-rec)
                                               (list-objects file))))
    (let ((shaders (list-shaders file)))
      (set! the-strings (append the-strings (map shader-code shaders)))
      (set! the-strings (append the-strings (map shader-name shaders))))

    (set! the-strings (append the-strings (map hans-program-name
                                               (hans-file-programs file))))
    (write-strings writer the-strings))

  (define (do-object-data writer file)
    (write-object-data writer (map hans-object-data (list-objects file))))

  (define (do-resources writer file)
    (define (combine-resources resource-lists)
      (fold (lambda (resource-list result)
              (fold (lambda (key out)
                      (assq-set! out key (+ (assoc-ref out key)
                                            (assoc-ref resource-list key))))
                    result
                    (map car resource-list)))
            (fold (lambda (key summed)
                    (acons key 0 summed))
                  '() (delete-duplicates
                        (fold (lambda (alist keys)
                                (append keys (map car alist)))
                              '() resource-lists)))
            resource-lists))

    (write-resources writer
      (combine-resources (map hans-object-resources (list-objects file)))))

  (let* ((writer (make-hans-file-writer 16384))
         (bytes  (fold (lambda (pass total)
                         (+ total (pass writer file))) 0 (list do-libraries
                                                               do-objects
                                                               do-parameters
                                                               do-programs
                                                               do-resources
                                                               do-object-data
                                                               do-shaders
                                                               do-fbos
                                                               do-strings))))
    (hans-file-write writer (assq-ref options 'output))
    file))

(define* (hans-compile file options #:optional passes)
  "Compile a hans-file"
  (define default-passes (list resolve-library-path-pass
                               object-resources-pass
                               validate-connections-pass
                               topological-sort-pass
                               validate-shader-pass
                               create-requested-resources-pass))
  (if (not (hans-file? file))
    (exit-with-error "Unknown file type")
    (begin
      (for-each (lambda (compiler-pass)
                  (set! file (compiler-pass file options)))
                (if (equal? passes #f) default-passes passes))
      file)))