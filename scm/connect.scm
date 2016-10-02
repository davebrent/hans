(define-module (hans connect)
  :export (make-midi-out
           midi-out-ports
           midi-out-open
           midi-out-send
           midi-out-close))

(load-extension "libhansconnect" "scm_init_connect_module")
