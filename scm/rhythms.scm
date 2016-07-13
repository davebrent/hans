(define-module (hans rhythms)
  :export (make-euclidean-rhythm
           make-binary-rhythm))

(define (flatten lst)
  "Recursively flatten a list"
  (define (perform output item)
    (cond ((null? item) output)
          (else (perform (append output (car item)) (cdr item)))))
  (perform '() lst))

(define (left-fill lst num)
  (append (make-list num 0) lst))

(define (int->bin num)
  "Convert an integer into a binary list"
  (map string->number (map string (string->list (number->string num 2)))))

(define (bin->int bin)
  "Convert a binary list back to an integer"
  (string->number (string-join (map number->string bin) "") 2))

(define (make-binary-rhythm lst fill)
  (flatten (map (lambda (x) (left-fill x (- fill (length x))))
                (map int->bin lst))))

(define (make-euclidean-rhythm onsets steps)
  "Return rhythms with onset pattens spread as evenly possible"
  (define (zip a b)
    (map cons a b))

  (define (tail lst k)
    (list-tail lst (- (length lst) k)))

  (define (extract lst k)
    (list-head (list-tail lst k) (- (length lst) (* k 2))))

  (define (interleave seq1 seq2)
    (map (lambda (pair) (append (car pair) (cdr pair)))
         (zip seq1 seq2)))

  ;; To create rhythms exactly as described in Godfried Toussaint's paper the
  ;; subtraction method is instead of the usual (euclid a (modulo a b)) which
  ;; arrives to the solution a bit to quickly...
  (define (euclid seq greater smaller iteration)
    (cond ((and (<= smaller 1) (>= iteration 1)) seq)
          (else (begin
            (let ((next-seq (append (interleave (list-head seq smaller)
                                                (tail seq smaller))
                                    (extract seq smaller)))
                  (next-greater (- greater smaller))
                  (next-iter (+ iteration 1)))
              (if (or (> smaller next-greater) (<= next-greater 0))
                (euclid next-seq smaller next-greater next-iter)
                (euclid next-seq next-greater smaller next-iter)))))))

  (let* ((zeros (- steps onsets))
         (initial (append (make-list onsets '(1)) (make-list zeros '(0)))))
    (flatten (if (> onsets zeros) (euclid initial onsets zeros 0)
                                  (euclid initial zeros onsets 0)))))
