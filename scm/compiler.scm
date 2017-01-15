(define-module (hans compiler)
  :use-module (hans common)
  :use-module (hans compiler shared)
  :use-module (hans compiler passes assign-graph-id)
  :use-module (hans compiler passes backend)
  :use-module (hans compiler passes normalize-args)
  :use-module (hans compiler passes configure-objects)
  :use-module (hans compiler passes register-allocation)
  :use-module (hans compiler passes resolve-library-paths)
  :use-module (hans compiler passes topological-sort)
  :use-module (hans compiler passes validate-connections)
  :use-module (hans compiler passes validate-modulators)
  :use-module (hans compiler passes validate-plugins)
  :use-module (hans compiler passes validate-shaders)
  :export (hans-compile))

(define* (hans-compile config programs #:optional passes)
  (let* ((output (make-hans-object 'engine-data '()))
         (default-passes `(,assign-graph-id-pass
                           ,normalize-args-pass
                           ,resolve-library-paths-pass
                           ,validate-plugins-pass
                           ,validate-modulators-pass
                           ,configure-objects-pass
                           ,validate-connections-pass
                           ,validate-shader-pass
                           ,topological-sort-pass
                           ,register-allocation-pass
                           ,backend-pass)))
    (for-each (lambda (compiler-pass)
                (compiler-pass programs output config))
              (if (equal? passes #f) default-passes passes))
    output))
