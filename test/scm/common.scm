(define-module (hans test common)
  #:use-module (srfi srfi-64)
  #:use-module (hans common))

(test-begin "test-common")

;; Simple objects
(let* ((obj (make-hans-primitive 'settings '()))
       (a (hans-primitive-get obj)))
  (assq-set! a 'channels 2)
  (assq-set! a 'blocksize 512)
  (assq-set! a 'samplerate 44100)
  (assq-set! a 'width 640)
  (assq-set! a 'height 360)
  (set-hans-primitive! obj a)

  (let ((b (hans-primitive-get obj)))
    (test-equal 2     (assq-ref b 'channels))
    (test-equal 512   (assq-ref b 'blocksize))
    (test-equal 44100 (assq-ref b 'samplerate))
    (test-equal 640   (assq-ref b 'width))
    (test-equal 360   (assq-ref b 'height))))

;; Nested objects
(let* ((obj (make-hans-primitive 'modulator '()))
       (a (hans-primitive-get obj))
       (b (assq-ref a 'source)))
  (test-equal 0 (assq-ref b 'object))
  (test-equal 0 (assq-ref b 'parameter))
  (test-equal 0 (assq-ref b 'component))

  (assq-set! b 'parameter 17)
  (assq-set! a 'source b)
  (set-hans-primitive! obj a)

  (let* ((c (hans-primitive-get obj))
         (d (assq-ref c 'source)))
    (test-equal 17 (assq-ref d 'parameter))))

;; Listed objects
(let* ((engine-obj (make-hans-primitive 'engine-data '()))
       (engine-data (hans-primitive-get engine-obj))
       (object-1 (make-hans-primitive 'object '()))
       (object-data-1 (hans-primitive-get object-1))
       (object-2 (make-hans-primitive 'object '()))
       (object-data-2 (hans-primitive-get object-2)))
  (assq-set! object-data-1 'name 12)
  (assq-set! object-data-2 'name 13)
  (assq-set! engine-data 'objects `(,object-data-1 ,object-data-2))
  (set-hans-primitive! engine-obj engine-data)

  (let* ((data (hans-primitive-get engine-obj))
         (objects (assq-ref data 'objects)))
    (test-equal 2 (length objects))
    (test-equal 12 (assq-ref (list-ref objects 0) 'name))
    (test-equal 13 (assq-ref (list-ref objects 1) 'name))))
