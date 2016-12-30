(define-module (hans common)
  :use-module (sxml simple)
  :use-module (hans extension)
  :export (hans-hash
           make-frame
           make-hans-object
           hans-object?
           hans-object-get
           set-hans-object!
           hans-objects))

(hans-load-extension "libhanscommon" "scm_init_common_module")

(define (normalize value)
  (let ((tag (car value))
        (attrs (car (cdr value)))
        (child (cdr (cdr value))))
    (if (not (eq? (car attrs) '@))
      (cons tag attrs)
      (cons tag child))))

(define (list-tag? tag)
  (string-prefix? "value" (symbol->string tag)))

(define (contains-list? child)
  (and (not (null? child))
       (symbol? (car child))
       (list-tag? (car (normalize child)))))

(define (deserialize-field value)
  (cond ((null? value)
          '())
        ((symbol? (car value))
          (let* ((field (normalize value))
                 (tag (car field))
                 (child (cdr field)))
            (if (list-tag? tag)
              (deserialize-field child)
              (cons tag (if (contains-list? child)
                          `(,(deserialize-field child))
                          (deserialize-field child))))))
        ((list? (car value))
          (map deserialize-field value))
        ((string? (car value))
          (string->number (car value)))))

(define (alist? value)
  (and (list? value)
       (>= (length value) 1)
       (pair? (car value))
       (symbol? (car (car value)))))

(define (complex-list? value)
  (and (list? value)
       (>= (length value) 1)
       (alist? (car value))))

(define (serialize-value value)
  (cond ((number? value)
          (number->string value))
        ((complex-list? value)
          (let ((i 0))
            (map (lambda (alist)
                   (let* ((tag (string->symbol
                                 (string-append "value" (number->string i))))
                          (value (serialize-value alist))
                          (serialized (append `(,tag) `(,value))))
                     (set! i (+ i 1))
                     serialized))
                 value)))
        ((alist? value)
          (map (lambda (pair)
                 `(,(car pair) ,(serialize-value (cdr pair))))
               value))
        ((list? value)
          (map serialize-value value))
        (else value)))

(define (deserialize-object buffer)
  (let* ((data (xml->sxml buffer #:trim-whitespace? #t))
         (body (list-ref data 2)))
    (car (assq-ref `(,(deserialize-field body)) 'cereal))))

(define (serialize-object data)
  (let ((body `((cereal
                ,(append '(value0)
                         (map (lambda (pair)
                                (let ((value (serialize-value (cdr pair))))
                                  (append `(,(car pair))
                                          (if (list? value)
                                            value
                                            `(,value)))))
                              data))))))
    (string-append
      "<?xml version=\"1.0\" encoding=\"utf-8\"?>"
      (call-with-output-string (lambda (port)
                                 (sxml->xml body port))))))

(define (hans-object-get obj)
  (deserialize-object (%hans-object-get obj)))

(define (set-hans-object! obj data)
  (%set-hans-object! obj (serialize-object data)))
