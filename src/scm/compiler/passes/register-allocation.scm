(define-module (hans compiler passes register-allocation)
  :use-module (hans compiler shared)
  :use-module (hans patcher)
  :export (register-allocation-pass))

(define (register-allocation-pass programs output options)
  (define (mark-readonly val)
    (lambda (reg)
      (append reg `(,val))))

  (define (mark-graph graph)
    (lambda (reg)
      (append `(,(hans-graph-id graph)) reg)))

  ;; TODO: Graph coloring
  (for-each-graph (lambda (graph)
                    (let ((conns (hans-graph-connections graph)))
                      (for-each (lambda (obj)
                                  (let ((i 0)
                                        (outs '())
                                        (ins '())
                                        (id (hans-object-instance-id obj)))
                                    (for-each (lambda (conn)
                                      (let ((source (list-ref conn 0))
                                            (outlet (list-ref conn 1))
                                            (sink   (list-ref conn 2))
                                            (inlet (list-ref conn 3)))
                                        (if (eq? source id)
                                          (set! outs (append outs (list
                                            `(,outlet ,i)))))
                                        (if (eq? sink id)
                                          (set! ins (append ins (list
                                            `(,inlet ,i)))))
                                        (set! i (+ i 1)))
                                    ) conns)
                                    (set-hans-object-registers! obj
                                      (map (mark-graph graph)
                                        (append
                                          (map (mark-readonly #t) ins)
                                          (map (mark-readonly #f) outs))))))
                                (hans-graph-objects graph))))
                  programs)
  output)
