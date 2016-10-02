(define-module (hans engine)
  :export (make-engine
           set-engine-program!
           engine-run
           engine-destroy))

(load-extension "libhansengine" "scm_init_engine_module")
