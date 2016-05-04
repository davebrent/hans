(define-module (hans modules snd-utils objects))
(use-modules (hans objects))

(define library "libhans.test.all")

(define-public (test-graphics)
  (audio-object "test-graphics"
    library
    "A test module for hans graphics"
    (list
      (parameter 'g1 "Test graphics parameter 1" 1 '(1))
      (parameter 'g2 "Test graphics parameter 2" 2 '(3 2)))))

(define-public (test-audio)
  (audio-object "test-audio"
    library
    "A test module for hans audio"
    (list
      (parameter 'a1 "Test audio parameter 1" 1 '(1))
      (parameter 'a2 "Test audio parameter 2" 2 '(3 2)))))
