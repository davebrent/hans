(define-module (hans test patterns)
  #:use-module (srfi srfi-64)
  #:use-module (hans patterns))

(test-begin "test-patterns")

(let ((pattern (make-pseq '(1 2 3) 3)))
  (test-equal '(1 2 3 1 2 3 1 2 3) (pattern->list pattern)))

(let ((pattern (make-pseq '(1 2 3) 2)))
  (test-equal 1 (pattern))
  (test-equal 2 (pattern))
  (test-equal 3 (pattern))
  (test-equal 1 (pattern)))

(let ((pattern (make-pseq '(1 2 3) 1)))
  (test-equal 1 (pattern))
  (test-equal 2 (pattern))
  (test-equal 3 (pattern))
  (test-equal #t (pattern-stopped? (pattern))))

(let ((pattern (make-prange 3 2)))
  (test-equal 0 (pattern))
  (test-equal 1 (pattern))
  (test-equal 2 (pattern))
  (test-equal 0 (pattern)))

(let ((pattern (make-pconcat (make-pseq '(10 11) 1)
                             (make-pseq '(12 13) 1) 1)))
  (test-equal 10 (pattern))
  (test-equal 11 (pattern))
  (test-equal 12 (pattern))
  (test-equal 13 (pattern)))
