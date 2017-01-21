(define-module (hans engine)
  :use-module (hans extension)
  :export (set-engine-program!
           set-engine-player!
           set-engine-parameter!
           engine-open
           engine-close
           engine-run
           engine-tick
           engine-capture
           engine-record-start
           engine-record-stop
           engine-player-start
           engine-player-stop))

(hans-load-extension "libhansengine" "scm_init_engine_module")
