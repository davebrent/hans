(define-module (hans control)
  :use-module (srfi srfi-9)
  :use-module (srfi srfi-9 gnu)
  :use-module (hans utils)
  :export (make-midi-out
           midi-out-ports
           midi-out-open
           midi-out-send
           midi-out-close))

(load-extension "libhanscontrol" "scm_init_control_module")
