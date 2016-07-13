(define-module (hans objects)
  :use-module (srfi srfi-9)
  :use-module (srfi srfi-9 gnu)
  :use-module (srfi srfi-1)
  :export (parameter
           parameter?
           parameter-name
           parameter-help
           parameter-components
           parameter-value

           fbo-attachment
           fbo-attachment?
           fbo-attachment-type
           fbo-attachment-help
           fbo-attachment-width
           fbo-attachment-height
           fbo-attachment-components

           fbo
           fbo?
           fbo-stencil-buffer
           fbo-attachments

           shader
           shader?
           shader-name
           shader-type
           shader-code
           vertex-shader?
           fragment-shader?

           audio-buffer
           audio-buffer-name
           audio-buffer?
           audio-buffer-channels
           audio-buffer-size

           object-record
           object-record?
           object-record-type
           object-record-library
           set-object-record-library!
           object-record-name
           object-record-parameters
           object-record-help
           object-record-shaders
           object-record-fbo
           object-record-audio-buffers

           audio-object
           audio-object?
           graphics-object
           graphics-object?))

(define-record-type <audio-buffer>
  (audio-buffer name channels size)
  audio-buffer?
  (name     audio-buffer-name)
  (channels audio-buffer-channels)
  (size     audio-buffer-size))

(define-record-type <parameter>
  (parameter name help components value)
  parameter?
  (name       parameter-name)
  (help       parameter-help)
  (components parameter-components)
  (value      parameter-value))

(define-record-type <fbo-attachment>
  (fbo-attachment type help width height components)
  fbo-attachment?
  (type       fbo-attachment-type)
  (help       fbo-attachment-help)
  (width      fbo-attachment-width)
  (height     fbo-attachment-height)
  (components fbo-attachment-components))

(define-record-type <fbo>
  (fbo stencil-buffer attachments)
  fbo?
  (stencil-buffer fbo-stencil-buffer)
  (attachments    fbo-attachments))

(define-record-type <shader>
  (shader type name code)
  shader?
  (type shader-type)
  (name shader-name)
  (code shader-code))

(set-record-type-printer! <shader>
  (lambda (record port)
    (write-char #\[ port)
    (display (shader-type record) port)
    (display ":" port)
    (display (shader-name record) port)
    (write-char #\] port)))

(define (vertex-shader? shdr)
  (and (shader? shdr) (eq? (shader-type shdr) 'vertex)))

(define (fragment-shader? shdr)
  (and (shader? shdr) (eq? (shader-type shdr) 'fragment)))

(define-record-type <object-record>
  (object-record type name library help parameters shaders fbo audio-buffers)
  object-record?
  (type          object-record-type)
  (name          object-record-name)
  (library       object-record-library set-object-record-library!)
  (help          object-record-help)
  (parameters    object-record-parameters)
  (shaders       object-record-shaders)
  (fbo           object-record-fbo)
  (audio-buffers object-record-audio-buffers))

(set-record-type-printer! <object-record>
  (lambda (record port)
    (write-char #\[ port)
    (display (object-record-type record) port)
    (write-char #\: port)
    (display (object-record-name record) port)
    (write-char #\] port)))

(define (audio-object name library help parameters buffers)
  (object-record 'audio name library help parameters '() '() buffers))

(define (audio-object? rec)
  (and (object-record? rec) (equal? (object-record-type rec) 'audio)))

(define (graphics-object name library help parameters shaders fbo)
  (object-record 'graphics name library help parameters shaders fbo '()))

(define (graphics-object? rec)
  (and (object-record? rec) (equal? (object-record-type rec) 'graphics)))
