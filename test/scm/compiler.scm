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

(define SETTINGS `((width  . 640)
                   (height . 360)))

(define OBJECTS `((test-object-1     . ,test-object-1)
                  (test-object-2     . ,test-object-2)
                  (test-gfx-object-1 . ,test-gfx-object-1)
                  (test-gfx-object-2 . ,test-gfx-object-2)))

(define opts `((output . "test.hans")))

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
    (assign-graph-id-pass `(,pgm-1 ,pgm-2) ng opts)
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
    (normalize-args-pass `(,pgm) ng opts)
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
                           (resolve-library-paths-pass `(,pgm) ng opts))
                         (lambda (key . args)
                           (test-equal #t (string-prefix? "Unable to find "
                                                          (car args)))
                           (set! ex #t)))
    (test-equal #t ex)))

; (begin
;   ;; Getting object resources
;   (let ((p1 (make-test-program "test-1" `((foo . bar))))
;         (ng (make-hans-primitive 'engine-data '()))
;         (ex #f))
;     (catch 'compileerror (lambda ()
;                            (configure-objects-pass `(,p1) ng opts))
;                          (lambda (key . args)
;                            (set! ex #t)))
;     (test-equal #t ex)))

(begin
  ;; Register allocation pass
  (let* ((env   (make-environment SETTINGS OBJECTS))
         (obj-1 (env 'create 'test-object-1 '() '(0 0)))
         (obj-2 (env 'create 'test-object-2 '() '(0 0)))
         (graph (make-audio-graph (env 'connect obj-1 0 obj-2 0)))
         (pgm   (make-program "foobar" graph))
         (ng    (make-hans-primitive 'engine-data '())))

    (set-hans-graph-id! graph 999)
    (register-allocation-pass `(,pgm) ng opts)

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
    (topological-sort-pass `(,pgm) ng opts)
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
      (test-equal `((filepath . 14377400405548874911))
                  (caar res)))))

(begin
  ;; Emitting objects
  (let* ((env   (make-environment SETTINGS OBJECTS))
         (obj-1 (env 'create 'test-object-1 '() '(0 0)))
         (obj-2 (env 'create 'test-object-2 '() '(0 0)))
         (graph (make-audio-graph (env 'connect obj-1 0 obj-2 0)))
         (pgm   (make-program "foobar" graph)))
    (let ((res (emit-objects `(,pgm))))
      (test-equal `((id   . ,(hans-object-instance-id obj-1))
                    (type . 0)
                    (name . 5602427413976156184))
                  (caar res)))))

(begin
  ;; Emitting arguments
  (let* ((env   (make-environment SETTINGS OBJECTS))
         (obj-1 (env 'create 'test-object-1 `((foo . 100)) '(0 0)))
         (obj-2 (env 'create 'test-object-2 `((baz . "bar")) '(0 0)))
         (graph (make-audio-graph (env 'connect obj-1 0 obj-2 0)))
         (pgm   (make-program "foobar" graph)))
    (let* ((res (emit-arguments `(,pgm)))
           (data (car res))
           (strings (cdr res)))
      (test-equal `(,"foo" ,"bar" ,"baz") strings)
      (test-equal `(1 1) (assq-ref data 'lengths))
      (test-equal `(0 1) (assq-ref data 'offsets))
      (test-equal `(((type    . 1)
                     (name    . 14834356025302342401)
                     (boolean . #f)
                     (number  . 100)
                     (string  . 0))
                    ((type    . 2)
                     (name    . 10533334703418180981)
                     (boolean . #f)
                     (number  . 0)
                     (string  . 17545852598994811774)))
                  (assq-ref data 'arguments))))

  ;; Emitting string arguments
  (let* ((env   (make-environment SETTINGS OBJECTS))
         (obj-1 (env 'create 'test-object-1 `((foo . "james")) '(0 0)))
         (obj-2 (env 'create 'test-object-2 `((baz . "brown")) '(0 0)))
         (graph (make-audio-graph (env 'connect obj-1 0 obj-2 0)))
         (pgm-1 (make-program "foobar" graph)))

      (let* ((res (emit-arguments `(,pgm-1)))
             (data (car res)))
        ;; `(brown james)
        (test-equal `(15835153999929349215 7640303621047919708)
                    (sort (map (lambda (arg)
                                 (assq-ref arg 'string))
                               (assq-ref data 'arguments))
                          >)))))

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
                  (env 'modulate obj-1 'a 0 obj-1 'b 0 0 1)
                  (env 'modulate obj-1 'b 0 obj-1 'a 0 0 1)))
         (pgm   (make-program "foobar" graph mods)))
    (set-hans-object-resources! obj-1 `((ring-buffer . "bar")))
    (let ((res (emit-modulators `(,pgm))))
      (test-equal `((source (object . ,(hans-object-instance-id obj-1))
                            (parameter . 510903276987443985)
                            (component . 0))
                    (dest (object . ,(hans-object-instance-id obj-1))
                          (parameter . 16900879642891266615)
                          (component . 0))
                    (offset . 0)
                    (scale . 1))
                  (caar res)))))

(begin
  ;; Emitting parameters
  (let* ((env   (make-environment SETTINGS OBJECTS))
         (obj-1 (env 'create 'test-gfx-object-1 '() '(0 0)))
         (obj-2 (env 'create 'test-gfx-object-2 '() '(0 0)))
         (graph (make-graphics-graph (env 'connect obj-1 0 obj-2 0)))
         (pgm   (make-program "foobar" graph)))
    (let ((res (emit-parameters `(,pgm))))
      (test-equal `(((object . ,(hans-object-instance-id obj-1))
                     (name   . ,(hans-hash "a"))
                     (size   . 1)
                     (offset . 0))
                    ((object . ,(hans-object-instance-id obj-1))
                     (name   . ,(hans-hash "b"))
                     (size   . 1)
                     (offset . 1))
                    ((object . ,(hans-object-instance-id obj-1))
                     (name   . ,(hans-hash "c"))
                     (size   . 1)
                     (offset . 2))
                    ((object . ,(hans-object-instance-id obj-2))
                     (name   . ,(hans-hash "d"))
                     (size   . 1)
                     (offset . 3))
                    ((object . ,(hans-object-instance-id obj-2))
                     (name   . ,(hans-hash "e"))
                     (size   . 1)
                     (offset . 4))
                    ((object . ,(hans-object-instance-id obj-2))
                     (name   . ,(hans-hash "f"))
                     (size   . 1)
                     (offset . 5)))
                  (car res)))))

(begin
  ;; Emitting parameter values
  (let* ((env   (make-environment SETTINGS OBJECTS))
         (obj-1 (env 'create 'test-gfx-object-1 '() '(0 0)))
         (obj-2 (env 'create 'test-gfx-object-2 '() '(0 0)))
         (graph (make-graphics-graph (env 'connect obj-1 0 obj-2 0)))
         (pgm   (make-program "foobar" graph)))
    (let ((res (emit-parameters-values `(,pgm))))
      (test-equal `(0.08 1.39 0.43 2.08 2.39 2.43) (car res)))))

(begin
  ;; Emitting chains
  (let* ((env   (make-environment SETTINGS OBJECTS))
         (snd-1-1 (env 'create 'test-object-1 '() '(0 0)))
         (snd-2-1 (env 'create 'test-object-2 '() '(0 0)))
         (gfx-1-1 (env 'create 'test-gfx-object-1 '() '(0 0)))
         (gfx-2-1 (env 'create 'test-gfx-object-1 '() '(0 0)))
         (snd-g-1 (make-audio-graph    (env 'connect snd-1-1 0 snd-2-1 0)))
         (gfx-g-1 (make-graphics-graph (env 'connect gfx-1-1 0 gfx-2-1 0)))
         (pgm-1   (make-program "pg-1" snd-g-1 gfx-g-1))

         (snd-1-2 (env 'create 'test-object-1 '() '(0 0)))
         (snd-2-2 (env 'create 'test-object-2 '() '(0 0)))
         (gfx-1-2 (env 'create 'test-gfx-object-1 '() '(0 0)))
         (gfx-2-2 (env 'create 'test-gfx-object-1 '() '(0 0)))
         (snd-g-2 (make-audio-graph    (env 'connect snd-1-2 0 snd-2-2 0)))
         (gfx-g-2 (make-graphics-graph (env 'connect gfx-1-2 0 gfx-2-2 0)))
         (pgm-2   (make-program "pg-2" snd-g-2 gfx-g-2)))
    (let ((res (emit-chains `(,pgm-1 ,pgm-2))))
      (test-equal `(,(hans-object-instance-id snd-1-1)
                    ,(hans-object-instance-id snd-2-1)
                    ,(hans-object-instance-id gfx-1-1)
                    ,(hans-object-instance-id gfx-2-1)
                    ,(hans-object-instance-id snd-1-2)
                    ,(hans-object-instance-id snd-2-2)
                    ,(hans-object-instance-id gfx-1-2)
                    ,(hans-object-instance-id gfx-2-2)) (car res)))))

(begin
  ;; Emitting programs
  (let* ((env   (make-environment SETTINGS OBJECTS))
         (obj-1 (env 'create 'test-gfx-object-1 '() '(0 0)))
         (obj-2 (env 'create 'test-gfx-object-1 '() '(0 0)))
         (graph (make-graphics-graph (env 'connect obj-1 0 obj-2 0)))
         (pgm   (make-program "foobar" graph)))
    (set-hans-graph-id! graph 99)
    (let ((res (emit-programs `(,pgm))))
      (test-equal `((name . 15321041522486911382)
                    (graphics (id    . 99)
                              (start . 0)
                              (end   . 2))
                    (audio (id    . 0)
                           (start . 3)
                           (end   . 3)))
                  (caar res)))))

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

; (begin
;   ;; Backend pass
;   (let* ((obj-1  (env 'create 'test-gfx-object-1 '() '(0 0)))
;          (obj-2  (env 'create 'test-gfx-object-1 '() '(0 0)))
;          (graph  (make-graphics-graph (env 'connect obj-1 0 obj-2 0)))
;          (pgm    (make-program "foobar" graph))
;          (output (make-hans-primitive 'engine-data '())))
;     (set-hans-object-registers! obj-1 `((99 0 1 #f)))
;     (set-hans-object-registers! obj-2 `((99 1 1 #t)))
;     (set-hans-object-resources! obj-1 `((ring-buffer . "bar")))
;     (set-hans-graph-id! graph 99)
;     (let ((res (backend-pass `(,pgm) output opts)))
;       (test-equal #t #t))))
