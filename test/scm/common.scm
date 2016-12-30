(define-module (hans test common)
  #:use-module (srfi srfi-64)
  #:use-module (hans common))

(test-begin "test-common")

;; Simple objects
(let* ((obj (make-hans-object 'config '()))
       (a (hans-object-get obj)))
  (assq-set! a 'channels 2)
  (assq-set! a 'blocksize 512)
  (assq-set! a 'samplerate 44100)
  (assq-set! a 'width 640)
  (assq-set! a 'height 360)
  (set-hans-object! obj a)

  (let ((b (hans-object-get obj)))
    (test-equal 2     (assq-ref b 'channels))
    (test-equal 512   (assq-ref b 'blocksize))
    (test-equal 44100 (assq-ref b 'samplerate))
    (test-equal 640   (assq-ref b 'width))
    (test-equal 360   (assq-ref b 'height))))

;; Nested objects
(let* ((obj (make-hans-object 'modulator '()))
       (a (hans-object-get obj))
       (b (assq-ref a 'source)))
  (test-equal 0 (assq-ref b 'object))
  (test-equal 0 (assq-ref b 'parameter))
  (test-equal 0 (assq-ref b 'component))

  (assq-set! b 'parameter 17)
  (assq-set! a 'source b)
  (set-hans-object! obj a)

  (let* ((c (hans-object-get obj))
         (d (assq-ref c 'source)))
    (test-equal 17 (assq-ref d 'parameter))))

;; Listed objects
(let* ((engine-obj (make-hans-object 'engine-data '()))
       (engine-data (hans-object-get engine-obj))
       (object-1 (make-hans-object 'object '()))
       (object-data-1 (hans-object-get object-1))
       (object-2 (make-hans-object 'object '()))
       (object-data-2 (hans-object-get object-2)))
  (assq-set! object-data-1 'size 12)
  (assq-set! object-data-2 'size 13)
  (assq-set! engine-data 'objects `(,object-data-1 ,object-data-2))
  (set-hans-object! engine-obj engine-data)

  (let* ((data (hans-object-get engine-obj))
         (objects (assq-ref data 'objects)))
    (test-equal 2 (length objects))
    (test-equal 12 (assq-ref (list-ref objects 0) 'size))
    (test-equal 13 (assq-ref (list-ref objects 1) 'size))))
