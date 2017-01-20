(define-module (hans compiler passes configure-objects)
  :use-module (srfi srfi-1)
  :use-module (hans common)
  :use-module (hans compiler shared)
  :use-module (hans compiler passes backend)
  :use-module (hans objects)
  :use-module (hans patcher)
  :use-module (hans utils)
  :export (configure-objects-pass))

(define (configure-objects-pass programs output options)
  ;; Allow objects to request resources, from C++, based on their arguments
  (let* ((engine-obj   (make-hans-primitive 'engine-data '()))
         (args-obj     (make-hans-primitive 'arguments '()))
         (engine-tpl   (hans-primitive-get engine-obj))
         (library-data (emit-libraries programs))
         (object-data  (emit-objects programs))
         (args-data    (emit-arguments programs))
         (libraries    (car library-data))
         (objects      (car object-data))
         (arguments    (car args-data))
         (strings      (make-strings (append (cdr library-data)
                                             (cdr object-data)
                                             (cdr args-data)))))

    (set! engine-tpl (assq-set! engine-tpl 'strings strings))
    (set! engine-tpl (assq-set! engine-tpl 'objects objects))
    (set! engine-tpl (assq-set! engine-tpl 'plugins libraries))
    (set-hans-primitive! engine-obj engine-tpl)
    (set-hans-primitive! args-obj arguments)

    (let ((states (%configure-objects engine-obj args-obj)))
      (for-each (lambda (data)
                  (let ((obj (car data))
                        (state-resources (cdr data)))
                    (if (null? (car state-resources))
                      (throw 'compileerror "Invalid object state"))

                    (set-hans-object-data! obj (car state-resources))
                    (set-hans-object-resources! obj (cdr state-resources))))
                (map-in-order cons (list-objects programs) states)))))
