(define-module (hans utils)
  :use-module (srfi srfi-1)
  :use-module (srfi srfi-9)
  :use-module (ice-9 rdelim)
  :export (print
           rstrip
           slurp-file
           exit-with-error
           record->alist))

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
