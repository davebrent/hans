(define-module (hans compiler passes resolve-library-paths)
  :use-module (srfi srfi-1)
  :use-module (hans compiler shared)
  :use-module (hans extension)
  :use-module (hans os)
  :use-module (hans objects)
  :use-module (hans patcher)
  :export (resolve-library-paths-pass))

(define (get-search-paths options)
  ;; Creates a list of all library search paths
  (let ((from-options (assq-ref options 'library-paths))
        (from-env (string-split (os-getenv "HANS_LIBRARY_PATH" "") #\:)))
    (append
      (if (eq? from-options #f) '() from-options)
      from-env
      `(,CMAKE-LIBRARY-OUTPUT-DIRECTORY
        ,(string-append CMAKE-INSTALL-PREFIX "/lib")))))

(define (resolve-library name paths)
  ;; Returns the location of a library or throws an error
  (let* ((names (map (lambda (ext) (string-append name ext))
                     '(".dylib" ".so")))
         (possible (fold (lambda (path all)
                           (append all (map (lambda (library)
                                              (os-path-join path library))
                                            names))) '() paths))
         (result (filter file-exists? possible)))
    (if (equal? result '())
      (throw 'compileerror "Unable to find library -" name "in" possible)
      (car result))))

(define (resolve-library-paths-pass programs output options)
  (let* ((search-paths (get-search-paths options))
         (library-paths (map (lambda (name)
                               (cons name (resolve-library name search-paths)))
                             (list-libraries programs))))
    (for-each-object (lambda (object)
      (let* ((rec (hans-object-rec object))
             (lib (assq-ref library-paths (object-record-library rec))))
        (if (not (eq? #f lib))
          (set-object-record-library! rec lib))))
      programs)
    output))
