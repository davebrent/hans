(define-module (hans engine)
  :use-module (hans extension)
  :export (make-engine
           set-engine-program!
           engine-run
           engine-destroy))

(hans-load-extension "libhansengine" "scm_init_engine_module")
