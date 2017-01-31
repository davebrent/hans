(define-module (hans compiler passes normalize-args)
  :use-module (hans compiler shared)
  :use-module (hans patcher)
  :export (normalize-args-pass))

(define (normalize-args-pass programs output options)
  ;; Ensure any object arguments are converted to strings
  (for-each-object
    (lambda (obj)
      (let ((args (hans-object-args obj)))
        (if args
          (for-each (lambda (pair)
                      (if (symbol? (cdr pair))
                        (assq-set! args
                                   (car pair)
                                   (symbol->string (cdr pair))))) args))))
    programs)
  output)
