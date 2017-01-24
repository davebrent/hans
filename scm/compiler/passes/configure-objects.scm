(define-module (hans compiler passes configure-objects)
  :use-module (srfi srfi-1)
  :use-module (hans common)
  :use-module (hans compiler shared)
  :use-module (hans compiler passes backend)
  :use-module (hans objects)
  :use-module (hans patcher)
  :use-module (hans utils)
  :export (configure-objects-pass))

(define %graphics-object? (compose graphics-object? hans-object-rec))
(define %audio-object? (compose audio-object? hans-object-rec))

(define (%configure-objects-pass programs emit-graphs object-filter)
  (let* ((graphs-primitive (make-hans-primitive 'graphs '()))
         (plugins-primitive (make-hans-primitive 'plugins '()))
         (args-primitive (make-hans-primitive 'arguments '()))
         (strings-primitive (make-hans-primitive 'strings '()))

         (graphs-data (emit-graphs programs))
         (libraries-data (emit-libraries programs))
         (args-data (emit-arguments programs object-filter)))

    (assq-set! (car graphs-data) 'states '())
    (set-hans-primitive! graphs-primitive (car graphs-data))
    (set-hans-primitive! plugins-primitive (car libraries-data))
    (set-hans-primitive! args-primitive (car args-data))
    (set-hans-primitive! strings-primitive (make-strings
                                             (append
                                               (cdr graphs-data)
                                               (cdr libraries-data)
                                               (cdr args-data))))

    (let ((states (%configure-objects strings-primitive
                                      args-primitive
                                      graphs-primitive
                                      plugins-primitive)))

      (for-each (lambda (data)
                  (let ((obj (car data))
                        (state-resources (cdr data)))
                    (if (null? (car state-resources))
                      (throw 'compileerror "Invalid object state"))

                    (set-hans-object-data! obj (car state-resources))
                    (set-hans-object-resources! obj (cdr state-resources))))
                (map-in-order cons
                              (filter object-filter (list-objects programs))
                              states)))))

(define (configure-objects-pass programs output options)
  (%configure-objects-pass programs emit-audio-graphs %audio-object?)
  (%configure-objects-pass programs emit-graphics-graphs %graphics-object?))
