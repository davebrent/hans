(define-module (hans common)
  :use-module (sxml simple)
  :use-module (hans extension)
  :export (hans-hash
           make-hans-primitive
           hans-primitive?
           hans-primitive-stringify
           hans-primitive-get
           set-hans-primitive!
           hans-primitive-enum
           hans-primitives))

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
          (let ((res (string->number (car value))))
            (if (not res)
              (cond ((string=? (car value) "true") #t)
                    ((string=? (car value) "false") #f)
                    (else (car value)))
              res)))))

(define (alist? value)
  (and (list? value)
       (>= (length value) 1)
       (pair? (car value))
       (symbol? (car (car value)))))

(define (iterator? value)
  (and (list? value)
       (>= (length value) 1)
       (or (alist? (car value))
           (number? (car value))
           (string? (car value)))))

(define (serialize-value value)
  (cond ((number? value)
          (number->string value))
        ((boolean? value)
          (if value "true" "false"))
        ((string? value)
          value)
        ((iterator? value)
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

(define (deserialize-primitive buffer)
  (let* ((data (xml->sxml buffer #:trim-whitespace? #t))
         (body (list-ref data 2)))
    (car (assq-ref `(,(deserialize-field body)) 'cereal))))

(define (serialize-primitive data)
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

(define hans-primitive-stringify %hans-primitive-get)

(define (hans-primitive-get obj)
  (deserialize-primitive (%hans-primitive-get obj)))

(define (set-hans-primitive! obj data)
  (%set-hans-primitive! obj (serialize-primitive data)))
