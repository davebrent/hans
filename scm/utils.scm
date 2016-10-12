(define-module (hans utils)
  :use-module (srfi srfi-1)
  :use-module (srfi srfi-9)
  :use-module (ice-9 rdelim)
  :export (print
           for-n
           rstrip
           slurp-file
           exit-with-error
           record->alist
           rotate-left
           rotate-right
           shuffle))

(define (print . args)
  (define i 0)
  (define len (length args))
  (for-each (lambda (arg)
    (display arg)
    (set! i (+ i 1))
    (if (not (equal? i len))
      (display " "))) args)
  (newline))

(define (rstrip str)
  (if (string=? (string-take-right str 1) "\n")
    (string-drop-right str 1)
    str))

(define (exit-with-error . args)
  (apply print args)
  (exit 1))

(define (slurp-file path)
  (read-string (open-file path "r")))

(define (record->alist rec)
  "Recursively transforms a record into an associative list"
  (define (get-value value)
    (cond ((record? value) (record->alist value))
           ((list? value) (map get-value value))
           (else value)))

  (let ((rtd (record-type-descriptor rec)))
    (map (lambda (prop)
        (cons prop (get-value ((record-accessor rtd prop) rec))))
      (record-type-fields rtd))))

(define (for-n n proc)
  ;; Call a procedure N times
  (do ((i 0 (1+ i)))
      ((>= i n))
    (proc i)))

(define (rotate-left lst n)
  ;; Rotate a list to the left by N items
  ;; >>> (rotate-left (5 3 2) 1)
  ;; 3 2 5
  (if (eq? n 0)
    lst
    (rotate-left (append (cdr lst) (list (car lst)))
                 (- n 1))))

(define (rotate-right lst n)
  ;; Rotate a list to the right by N items
  ;; >>> (rotate-right (5 3 2) 1)
  ;; 2 5 3
  (if (eq? n 0)
    lst
    (rotate-right (append `(,(last lst)) (reverse (cdr (reverse lst))))
                  (- n 1))))

(define (shuffle lst)
  ;; Shuffle a list
  (map cdr (sort (map (lambda (x)
                        (cons (random 100) x))
                      lst)
                 (lambda (a b)
                   (< (car a) (car b))))))
