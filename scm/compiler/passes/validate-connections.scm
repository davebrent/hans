(define-module (hans compiler passes validate-connections)
  :use-module (hans compiler shared)
  :use-module (hans objects)
  :use-module (hans patcher)
  :export (validate-connections-pass))

(define (validate-connections-pass programs output options)
  ;; Checks that connections match the requested number of inlets & outlets

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
            (throw 'compileerror
                 (string-append "Invalid connection, ["
                                (number->string (hans-object-instance-id obj))
                                " "
                                (object-record-name (hans-object-rec obj))
                                "]")
                 result
                 resources))))
        objects)))
    programs)
  output)
