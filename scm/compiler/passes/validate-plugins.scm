(define-module (hans compiler passes validate-plugins)
  :use-module (srfi srfi-1)
  :use-module (hans compiler shared)
  :export (validate-plugins-pass))

(define (validate-plugins-pass programs output options)
  ;; Check plugins exist on the filesystem
  (let* ((libraries (list-libraries programs))
         (missing (filter (lambda (lib) (not (file-exists? lib))) libraries)))
    (if (> (length missing) 0)
      (throw 'compileerror "Unable to find libraries" missing)
      output)))
