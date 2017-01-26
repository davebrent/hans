(define-module (hans test compiler)
  #:use-module (srfi srfi-64)
  #:use-module (hans patcher)
  #:use-module (hans common)
  #:use-module (hans objects)
  #:use-module (hans compiler passes assign-graph-id)
  #:use-module (hans compiler passes normalize-args)
  #:use-module (hans compiler passes resolve-library-paths)
  #:use-module (hans compiler passes configure-objects)
  #:use-module (hans compiler passes register-allocation)
  #:use-module (hans compiler passes topological-sort)
  #:use-module (hans compiler passes backend))

;; Fixture data

(define (test-object-1 settings args)
  (audio-object "test-object-1"
    "test-library"
    "Example text"
    '()
    `(,(audio-buffer "buffer-1" 2 512))))

(define (test-object-2 settings args)
  (audio-object "test-object-2"
    "test-library"
    "Example text"
    '()
    `(,(audio-buffer "buffer-2" 1 256))))

(define (test-object-3 settings args)
  (audio-object "test-object-3"
    "test-library"
    "Example text"
    `(,(parameter 'freq 440))
    `(,(audio-buffer "buffer-2" 1 256))))

(define (test-gfx-object-1 settings args)
  (graphics-object
    "test-gfx-object"
    "test-gfx-library"
    ""
    `(,(parameter 'a 0.08)
      ,(parameter 'b 1.39)
      ,(parameter 'c 0.43))
    `(,(shader 'vertex "attractors/shaders/vert" "Some vertex code")
      ,(shader 'fragment "attractors/shaders/frag" "Some fragment code"))
    (fbo #t `(,(fbo-attachment 'color "Output data" 128 256 4)))))

(define (test-gfx-object-2 settings args)
  (graphics-object
    "test-gfx-object"
    "test-gfx-library"
    ""
    `(,(parameter 'd 2.08)
      ,(parameter 'e 2.39)
      ,(parameter 'f 2.43))
    `(,(shader 'vertex "attractors/shaders/vert" "Some vertex code")
      ,(shader 'fragment "attractors/shaders/frag" "Some fragment code"))
    (fbo #t `(,(fbo-attachment 'color "Output data" 128 256 4)))))

(define SETTINGS `((width      . 640)
                   (height     . 360)
                   (channels   . 2)
                   (samplerate . 44100)
                   (blocksize  . 64)))

(define OBJECTS `((test-object-1     . ,test-object-1)
                  (test-object-2     . ,test-object-2)
                  (test-object-3     . ,test-object-3)
                  (test-gfx-object-1 . ,test-gfx-object-1)
                  (test-gfx-object-2 . ,test-gfx-object-2)))

(test-begin "test-compiler")

(begin
  ;; Test assigning graph ids
  (let* ((env     (make-environment SETTINGS OBJECTS))
         (obj-1-1 (env 'create 'test-object-1 '() '(0 0)))
         (obj-2-1 (env 'create 'test-object-2 '() '(0 0)))
         (graph-1 (make-audio-graph (env 'connect obj-1-1 0 obj-2-1 0)))
         (pgm-1   (make-program "foobar" graph-1))

         (obj-1-2 (env 'create 'test-object-1 '() '(0 0)))
         (obj-2-2 (env 'create 'test-object-2 '() '(0 0)))
         (graph-2 (make-audio-graph (env 'connect obj-1-2 0 obj-2-2 0)))
         (pgm-2   (make-program "foobar" graph-2))

         (ng (make-hans-primitive 'engine-data '())))
    (assign-graph-id-pass `(,pgm-1 ,pgm-2) ng SETTINGS)
    (test-equal 0 (hans-graph-id (hans-program-audio-graph pgm-1)))
    (test-equal 1 (hans-graph-id (hans-program-graphics-graph pgm-1)))
    (test-equal 2 (hans-graph-id (hans-program-audio-graph pgm-2)))
    (test-equal 3 (hans-graph-id (hans-program-graphics-graph pgm-2)))))

(begin
  ;; Test normalizing arguments
  (let* ((env   (make-environment SETTINGS OBJECTS))
         (obj-1 (env 'create 'test-object-1 '((foo . bar)) '(0 0)))
         (obj-2 (env 'create 'test-object-2 '() '(0 0)))
         (graph (make-audio-graph (env 'connect obj-1 0 obj-2 0)))
         (pgm   (make-program "foobar" graph))
         (ng    (make-hans-primitive 'engine-data '())))
    (normalize-args-pass `(,pgm) ng SETTINGS)
    (test-equal "bar" (assq-ref (hans-object-args obj-1) 'foo))))

(begin
  ;; Resolving library paths
  (let* ((env   (make-environment SETTINGS OBJECTS))
         (obj-1 (env 'create 'test-object-1 '() '(0 0)))
         (obj-2 (env 'create 'test-object-2 '() '(0 0)))
         (graph (make-audio-graph (env 'connect obj-1 0 obj-2 0)))
         (pgm   (make-program "foobar" graph))
         (ng    (make-hans-primitive 'engine-data '()))
         (ex    #f))
    (catch 'compileerror (lambda ()
                           (resolve-library-paths-pass `(,pgm) ng SETTINGS))
                         (lambda (key . args)
                           (test-equal #t (string-prefix? "Unable to find "
                                                          (car args)))
                           (set! ex #t)))
    (test-equal #t ex)))

(begin
  ;; Register allocation pass
  (let* ((env   (make-environment SETTINGS OBJECTS))
         (obj-1 (env 'create 'test-object-1 '() '(0 0)))
         (obj-2 (env 'create 'test-object-2 '() '(0 0)))
         (graph (make-audio-graph (env 'connect obj-1 0 obj-2 0)))
         (pgm   (make-program "foobar" graph))
         (ng    (make-hans-primitive 'engine-data '())))

    (set-hans-graph-id! graph 999)
    (register-allocation-pass `(,pgm) ng SETTINGS)

    (let ((c1 (car (hans-object-registers obj-1)))
          (c2 (car (hans-object-registers obj-2))))

      (test-equal 999 (list-ref c1 0)) ;; graph ID
      (test-equal 0   (list-ref c1 1)) ;; object index
      (test-equal 0   (list-ref c1 2)) ;; register bin
      (test-equal #f  (list-ref c1 3)) ;; readonly

      (test-equal 999 (list-ref c2 0))
      (test-equal 0   (list-ref c2 1))
      (test-equal 0   (list-ref c2 2))
      (test-equal #t  (list-ref c2 3)))))

(begin
  ;; Topological sort pass
  (let* ((env   (make-environment SETTINGS OBJECTS))
         (obj-1 (env 'create 'test-object-1 '() '(0 0)))
         (obj-2 (env 'create 'test-object-1 '() '(0 0)))
         (obj-3 (env 'create 'test-object-1 '() '(0 0)))
         (graph (make-audio-graph (env 'connect obj-2 0 obj-3 0)
                                  (env 'connect obj-1 0 obj-2 0)))
         (pgm   (make-program "foobar" graph))
         (ng    (make-hans-primitive 'engine-data '())))
    (topological-sort-pass `(,pgm) ng SETTINGS)
    (let ((objs (map hans-object-instance-id (hans-graph-objects graph))))
      (test-equal `(,(hans-object-instance-id obj-1)
                    ,(hans-object-instance-id obj-2)
                    ,(hans-object-instance-id obj-3)) objs))))

(begin
  ;; Emitting libraries
  (let* ((env   (make-environment SETTINGS OBJECTS))
         (obj-1 (env 'create 'test-object-1 '() '(0 0)))
         (obj-2 (env 'create 'test-object-1 '() '(0 0)))
         (graph (make-audio-graph (env 'connect obj-1 0 obj-2 0)))
         (pgm   (make-program "foobar" graph)))
    (let ((res (emit-libraries `(,pgm))))
      (test-equal `((filepaths 14377400405548874911))
                  (car res)))))

(begin
  ;; Emitting programs
  (let* ((env   (make-environment SETTINGS OBJECTS))
         (gfx-obj-1 (env 'create 'test-gfx-object-1 '() '(0 0)))
         (gfx-obj-2 (env 'create 'test-gfx-object-1 '() '(0 0)))
         (gfx-obj-3 (env 'create 'test-gfx-object-1 '() '(0 0)))
         (snd-obj-1 (env 'create 'test-object-1 '() '(0 0)))
         (snd-obj-2 (env 'create 'test-object-1 '() '(0 0)))
         (snd-graph (make-audio-graph
                      (env 'connect snd-obj-1 0 snd-obj-2 0)))
         (gfx-graph (make-graphics-graph
                      (env 'connect gfx-obj-1 0 gfx-obj-2 0)
                      (env 'connect gfx-obj-2 0 gfx-obj-3 0)))
         (pgm-1 (make-program "foobar" gfx-graph snd-graph))
         (pgm-2 (make-program "bazbar" gfx-graph snd-graph)))
    (let ((res (emit-programs `(,pgm-1 ,pgm-2))))
      (test-equal `((names 15321041522486911382 7891287109127304821)
                    (audio
                      (objects ((id . 3) (name . 5602427413976156184))
                               ((id . 4) (name . 5602427413976156184)))
                      (indices 0 1 0 1)
                      (ranges ((start . 0) (end . 2))
                              ((start . 2) (end . 4)))
                      (states #f #f))
                    (graphics
                      (objects ((id . 0) (name . 373542606194174761))
                               ((id . 1) (name . 373542606194174761))
                               ((id . 2) (name . 373542606194174761)))
                      (indices 0 1 2 0 1 2)
                      (ranges ((start . 0) (end . 3))
                              ((start . 3) (end . 6)))
                      (states #f #f #f)))
                  (car res))
      (test-equal `("foobar"
                    "bazbar"
                    "test-object-1"
                    "test-gfx-object") (cdr res)))))

(begin
  ;; Emitting shaders
  (let* ((env   (make-environment SETTINGS OBJECTS))
         (obj-1 (env 'create 'test-gfx-object-1 '() '(0 0)))
         (obj-2 (env 'create 'test-gfx-object-1 '() '(0 0)))
         (graph (make-graphics-graph (env 'connect obj-1 0 obj-2 0)))
         (pgm   (make-program "foobar" graph)))
    (let ((res (emit-shaders `(,pgm))))
      (test-equal `((type . 0)
                    (name . 2905517049036341412)
                    (code . 13171141140566594271))
                  (caar res)))))

(begin
  ;; Emitting registers
  (let* ((env   (make-environment SETTINGS OBJECTS))
         (obj-1 (env 'create 'test-gfx-object-1 '() '(0 0)))
         (obj-2 (env 'create 'test-gfx-object-1 '() '(0 0)))
         (graph (make-graphics-graph (env 'connect obj-1 0 obj-2 0)))
         (pgm   (make-program "foobar" graph)))
    (set-hans-object-registers! obj-1 `((98 0 1 #f)))
    (set-hans-object-registers! obj-2 `((99 1 1 #t)))
    (let ((res (emit-registers `(,pgm))))
      (test-equal `((object   . ,(hans-object-instance-id obj-1))
                    (type     . 1)
                    (graph    . 98)
                    (index    . 0)
                    (bin      . 1)
                    (readonly . #f))
                  (caar res)))))

(begin
  ;; Emitting audio buffers
  (let* ((env   (make-environment SETTINGS OBJECTS))
         (obj-1 (env 'create 'test-object-1 '() '(0 0)))
         (obj-2 (env 'create 'test-object-1 '() '(0 0)))
         (graph (make-audio-graph (env 'connect obj-1 0 obj-2 0)))
         (pgm   (make-program "foobar" graph)))
    (let ((res (emit-audio-buffers `(,pgm))))
      (test-equal `((object   . ,(hans-object-instance-id obj-1))
                    (name     . 11925552888592014305)
                    (channels . 2)
                    (size     . 512))
                  (caar res)))))

(begin
  ;; Emitting ring buffers
  (let* ((env   (make-environment SETTINGS OBJECTS))
         (obj-1 (env 'create 'test-object-1 '() '(0 0)))
         (obj-2 (env 'create 'test-object-1 '() '(0 0)))
         (graph (make-audio-graph (env 'connect obj-1 0 obj-2 0)))
         (pgm   (make-program "foobar" graph)))
    (set-hans-object-resources! obj-1 `((ring-buffer . "bar")))
    (let ((res (emit-ring-buffers `(,pgm))))
      (test-equal `((producer . ,(hans-object-instance-id obj-1))
                    (name . 17545852598994811774)
                    (index . 0))
                  (caar res)))))

(begin
  ;; Emitting modulators
  (let* ((env   (make-environment SETTINGS OBJECTS))
         (obj-1 (env 'create 'test-gfx-object-1 '() '(0 0)))
         (obj-2 (env 'create 'test-gfx-object-1 '() '(0 0)))
         (graph (make-graphics-graph (env 'connect obj-1 0 obj-2 0)))
         (mods  (make-modulation
                  (env 'modulate obj-1 'a 0 obj-2 'b 0 0.25 1)
                  (env 'modulate obj-2 'c 0 obj-1 'b 0 0.4 2)))
         (pgm   (make-program "foobar" graph mods)))
    (set-hans-object-resources! obj-1 `((ring-buffer . "bar")))
    (let ((res (emit-modulators `(,pgm))))
      (test-equal `((audio (local)
                           (cross))
                    (graphics (local ((scale . 1)
                                      (offset . 0.25)
                                      (source . 0)
                                      (destination . 4))
                                     ((scale . 2)
                                      (offset . 0.4)
                                      (source . 5)
                                      (destination . 1)))
                              (cross)))
                  (car res)))))

(begin
  ;; Emitting parameters
  (let* ((env   (make-environment SETTINGS OBJECTS))
         (obj-1 (env 'create 'test-gfx-object-1 '() '(0 0)))
         (obj-2 (env 'create 'test-gfx-object-2 '() '(0 0)))
         (obj-3 (env 'create 'test-object-3 '() '(0 0)))
         (obj-4 (env 'create 'test-object-3 '() '(0 0)))
         (graph-1 (make-graphics-graph (env 'connect obj-1 0 obj-2 0)))
         (graph-2 (make-audio-graph (env 'connect obj-3 0 obj-4 0)))
         (pgm   (make-program "foobar" graph-1 graph-2)))
    (let ((res (emit-parameters `(,pgm))))
      (test-equal `((handles ((object . 3)
                              (name . 6729007284817213311)
                              (size . 1)
                              (offset . 0))
                             ((object . 2)
                              (name . 6729007284817213311)
                              (size . 1)
                              (offset . 1))
                             ((object . 0)
                              (name . 510903276987443985)
                              (size . 1)
                              (offset . 2))
                             ((object . 0)
                              (name . 16900879642891266615)
                              (size . 1)
                              (offset . 3))
                             ((object . 0)
                              (name . 15598372446745583797)
                              (size . 1)
                              (offset . 4))
                             ((object . 1)
                              (name . 1720370409040345145)
                              (size . 1)
                              (offset . 5))
                             ((object . 1)
                              (name . 8148669997605808657)
                              (size . 1)
                              (offset . 6))
                             ((object . 1)
                              (name . 6217145520856553898)
                              (size . 1)
                              (offset . 7)))
                    (buffer 440 440 0.08 1.39 0.43 2.08 2.39 2.43)
                    (split . 2))
                  (car res)))))

(begin
  ;; Emitting fbo attachments
  (let* ((env   (make-environment SETTINGS OBJECTS))
         (obj-1 (env 'create 'test-gfx-object-1 '() '(0 0)))
         (obj-2 (env 'create 'test-gfx-object-1 '() '(0 0)))
         (graph (make-graphics-graph (env 'connect obj-1 0 obj-2 0)))
         (pgm   (make-program "foobar" graph)))
    (let ((res (emit-fbos-attachments `(,pgm))))
      (test-equal `((type       . 0)
                    (width      . 128)
                    (height     . 256)
                    (components . 4))
                  (caar res)))))

(begin
  ;; Emitting fbos
  (let* ((env   (make-environment SETTINGS OBJECTS))
         (obj-1 (env 'create 'test-gfx-object-1 '() '(0 0)))
         (obj-2 (env 'create 'test-gfx-object-1 '() '(0 0)))
         (graph (make-graphics-graph (env 'connect obj-1 0 obj-2 0)))
         (pgm   (make-program "foobar" graph)))
    (let ((res (emit-fbos `(,pgm))))
      (test-equal `((object         . ,(hans-object-instance-id obj-1))
                    (stencil_buffer . #t)
                    (start          . 0)
                    (end            . 1))
                  (caar res)))))

(begin
  ;; Backend pass
  (let* ((env   (make-environment SETTINGS OBJECTS))
         (obj-1  (env 'create 'test-gfx-object-1 '() '(0 0)))
         (obj-2  (env 'create 'test-gfx-object-1 '() '(0 0)))
         (graph  (make-graphics-graph (env 'connect obj-1 0 obj-2 0)))
         (pgm    (make-program "foobar" graph))
         (output (make-hans-primitive 'engine-data '())))
    (set-hans-object-registers! obj-1 `((99 0 1 #f)))
    (set-hans-object-registers! obj-2 `((99 1 1 #t)))
    (set-hans-object-resources! obj-1 `((ring-buffer . "bar")))
    (set-hans-graph-id! graph 99)
    (let ((res (backend-pass `(,pgm) output SETTINGS)))
      (test-equal #t #t))))
