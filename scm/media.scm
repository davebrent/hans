(define-module (hans media)
  :use-module (hans extension)
  :export (image-encode))

(hans-load-extension "libhansmedia" "scm_init_hans_media_module")
