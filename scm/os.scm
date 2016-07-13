(define-module (hans os)
  :use-module (ice-9 ftw)
  :export (os-getenv
           os-path-join
           os-directory?
           os-list-directories))

;; Surely some of this exists in guile somewhere?

(define (os-getenv var default)
  "Return an environment variable or the default value"
  (let ((val (getenv var)))
    (if (equal? val #f) default val)))

(define (os-path-join . paths)
  "Join file path components (POSIX only)"
  (string-join paths file-name-separator-string 'infix))

(define (os-directory? path)
  "Returns true if path is a directory"
  (catch #t (lambda () (opendir path) #t)
            (lambda (key . args) #f)))

(define (os-list-directories path)
  "List all directories in path"
  (filter (lambda (component)
            (if (or (string=? component "..") (string=? component "."))
              #f
              (os-directory? (os-path-join path component))))
          (scandir path)))
