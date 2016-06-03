(define-module (hans test compiler)
  #:use-module (srfi srfi-64)
  #:use-module (hans compiler)
  #:use-module (hans patcher)
  #:use-module (hans objects)
  #:use-module (hans utils)
  #:use-module (hans modules snd-io objects)
  #:use-module (hans modules snd-loadfile objects)
  #:use-module (hans modules snd-oscillator objects)
  #:use-module (hans modules snd-utils objects)
  #:use-module (hans modules gfx-superformula objects)
  #:use-module (hans modules gfx-quad objects)
  #:use-module (hans modules gfx-filter objects))

(test-begin "test-compiler")

;; TODO: Mock these out?
(define hans-env (make-environment
  `((snd-loadfile     . ,snd-loadfile)
    (snd-in           . ,snd-in)
    (snd-out          . ,snd-out)
    (snd-oscillator   . ,snd-oscillator)
    (snd-gain         . ,snd-gain)
    (snd-pan          . ,snd-pan)
    (gfx-quad         . ,gfx-quad)
    (gfx-superformula . ,gfx-superformula)
    (gfx-filter       . ,gfx-filter))))

(define (compile-opts)
  '((verbose       . #t)
    (output        . "test-01.hans")
    (library-paths . ("/Users/dave/Projects/hans/build/lib"
                      "/vagrant/build/lib"))))

(define (make-test-program)
  (let ((loadfile     (hans-env 'create 'snd-loadfile     '() '(0 0)))
        (dac          (hans-env 'create 'snd-out          '() '(0 0)))
        (superformula (hans-env 'create 'gfx-superformula '() '(0 0)))
        (threshold    (hans-env 'create 'gfx-filter
                                        '((shader . threshold)) '(0 0))))
    (hans-program
      "test-01"
      (make-audio-graph
        (list loadfile dac)
        (list (hans-env 'connect loadfile 0 dac 0)))
      (make-graphics-graph
        (list superformula threshold)
        (list (hans-env 'connect superformula 0 threshold 0))))))

(define (make-shared-object-program)
  ;; An example of sharing an object between programs
  ;;
  ;; [adc]   [adc]
  ;;   |       |
  ;; [gain]  [gain]
  ;;   |       |
  ;; [dac]   [dac]
  (let ((adc-1 (hans-env 'create 'snd-in   `((channel . 0) (bus . 0)) '(50 0)))
        (dac-1 (hans-env 'create 'snd-out  `((channel . 0) (bus . 0)) '(50 40)))
        (adc-2 (hans-env 'create 'snd-in   `((channel . 0) (bus . 0)) '(50 0)))
        (dac-2 (hans-env 'create 'snd-out  `((channel . 0) (bus . 0)) '(50 40)))
        (gain  (hans-env 'create 'snd-gain `((amount . 0.5)) '(0 20)))
        (superformula (hans-env 'create 'gfx-superformula '() '(0 20))))

    (list
      (hans-program
        "test-02"
        (make-audio-graph
          (list gain dac-1 adc-1)
          (list (hans-env 'connect adc-1 0 gain 0)
                (hans-env 'connect gain 0 dac-1 0)))
        (make-graphics-graph
          (list superformula)
          '()))
      (hans-program
        "test-03"
        (make-audio-graph
          (list gain dac-2 adc-2)
          (list (hans-env 'connect adc-2 0 gain 0)
                (hans-env 'connect gain 0 dac-2 0)))
        (make-graphics-graph '() '())))))

(define (make-split-gain-program)
  ;;       [adc]
  ;;      /     \
  ;; [gain-l] [gain-r]
  ;;      \     /
  ;;       [dac]
  (define in-args  `((channel . 0) (channel . 1) (bus . 0)))
  (define out-args `((channel . 0) (channel . 1) (bus . 0)))
  (define gain-args `((amount . 0.5)))

  (let ((dac        (hans-env 'create 'snd-out  in-args   '(50 40)))
        (gain-left  (hans-env 'create 'snd-gain gain-args '(0 20)))
        (adc        (hans-env 'create 'snd-in   out-args  '(50 0)))
        (gain-right (hans-env 'create 'snd-gain gain-args '(100 20))))

    (hans-program
      "test-04"
      (make-audio-graph
        (list gain-left gain-right dac adc)
        (list (hans-env 'connect adc 0 gain-left 0)
              (hans-env 'connect adc 1 gain-right 0)
              (hans-env 'connect gain-right 0 dac 1)
              (hans-env 'connect gain-left 0 dac 0)))
      (make-graphics-graph '() '()))))

(define (topological-sort-pass-tests)
  (let* ((result (hans-compile (hans-file (list (make-split-gain-program)))
                                 (compile-opts)
                                 (list topological-sort-pass)))
         (objects (map-objects (compose object-record-name hans-object-rec)
                               result)))
    (test-equal '("snd-in" "snd-gain" "snd-gain" "snd-out") objects)))

(define (object-resources-pass-tests)
  (let* ((input (hans-file (list (make-split-gain-program))))
         (output (hans-compile input (compile-opts)
                                     (list resolve-library-path-pass
                                           object-resources-pass))))
    ;(map-objects (compose (lambda (rec)
    ;               (print (object-record-resources rec))
    ;               ) hans-object-rec) output)
    (test-equal #t #t)))

(define (validate-connections-pass-tests)
  ;(let* ((input (hans-file (list (make-split-gain-program))))
  (let* ((input (hans-file (make-shared-object-program)))
         (output (hans-compile input (compile-opts)
                                     (list topological-sort-pass
                                           resolve-library-path-pass
                                           object-resources-pass
                                           validate-connections-pass
                                           create-requested-resources-pass))))
    ;(map-objects (compose (lambda (rec)
    ;               (print (object-record-resources rec))
    ;               ) hans-object-rec) output)
    (test-equal #t #t)))

(topological-sort-pass-tests)
(object-resources-pass-tests)
(validate-connections-pass-tests)
