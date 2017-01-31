(define-module (hans compiler passes topological-sort)
  :use-module (srfi srfi-1)
  :use-module (hans compiler shared)
  :use-module (hans patcher)
  :export (topological-sort-pass))

(define (simplify-connections connections)
  ;; (pad source pad sink) -> (source sink)
  (delete-duplicates (map (lambda (conn)
                            (cons (list-ref conn 0) (list-ref conn 2)))
                          connections)))

(define (object-connections id connections)
  (delete-duplicates
    (map cdr (filter (lambda (conn) (eqv? (car conn) id)) connections))))

(define (graph->adjacency-list graph)
  ;; Transform a hans graph into a simpler adjacency list
  (let ((conns (simplify-connections (hans-graph-connections graph))))
    (map (lambda (object)
           (let ((id (hans-object-instance-id object)))
             (append (list id) (object-connections id conns))))
         (hans-graph-objects graph))))

(define (topological-sort-helper dag insert lookup)
  ;; Taken from tsort.scm from SLIB
  (if (null? dag)
    '()
    (let* ((adj-table (make-hash-table)) (sorted '()))
      (letrec ((visit (lambda (u adj-list)
                        ;; Color vertex u
                        (insert adj-table u 'colored)
                        ;; Visit uncolored vertices which u connects to
                        (for-each (lambda (v)
                                    (let ((val (lookup adj-table v)))
                                      (if (not (eq? val 'colored))
                                          (visit v (or val '())))))
                                  adj-list)
                        ;; Since all vertices downstream u are visited
                        ;; by now, we can safely put u on the output list
                        (set! sorted (cons u sorted)))))
        ;; Hash adjacency lists
        (for-each (lambda (def)
                    (insert adj-table (car def) (cdr def)))
                  (cdr dag))
        ;; Visit vertices
        (visit (caar dag) (cdar dag))
        (for-each (lambda (def)
                    (let ((val (lookup adj-table (car def))))
                      (if (not (eq? val 'colored))
                        (visit (car def) (cdr def)))))
                  (cdr dag)))
      sorted)))

(define (sort-graph graph)
  (let ((sorted (topological-sort-helper (graph->adjacency-list graph)
                                         hash-set! hash-ref))
        (objects (hans-graph-objects graph)))
    (set-hans-graph-objects!
      graph
      (map (lambda (id)
             (find (lambda (obj)
                     (equal? (hans-object-instance-id obj) id))
                   objects))
           sorted))))

(define (topological-sort-pass programs output options)
  (for-each-graph sort-graph programs)
  output)
