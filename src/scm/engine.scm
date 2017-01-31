(define-module (hans engine)
  :use-module (hans extension)
  :export (hans-hash
           set-engine-program!
           set-engine-parameter!
           engine-setup
           engine-destroy
           engine-run-forever
           engine-tick-graphics
           engine-tick-audio
           engine-capture
           engine-record-start
           engine-record-stop
           engine-player-start
           set-engine-player!
           engine-player-stop))

(hans-load-extension "libhans.scm.engine" "scm_init_hans_engine")
