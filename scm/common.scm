(define-module (hans common)
  :use-module (hans extension)
  :export (hans-hash))

(hans-load-extension "libhanscommon" "scm_init_common_module")
