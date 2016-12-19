(define-module (hans engine)
  :use-module (hans extension)
  :export (make-engine
           set-engine-program!
           engine-open
           engine-close
           engine-run
           engine-tick))

(hans-load-extension "libhansengine" "scm_init_engine_module")
