(define-module (hans engine)
  :use-module (hans extension)
  :export (set-engine-program!
           set-engine-frame!
           set-engine-parameter!
           engine-open
           engine-close
           engine-run
           engine-tick
           engine-capture
           engine-record-start
           engine-record-stop))

(hans-load-extension "libhansengine" "scm_init_engine_module")
