(define-module (hans compiler passes validate-modulators)
  :use-module (srfi srfi-1)
  :use-module (hans compiler shared)
  :use-module (hans objects)
  :use-module (hans patcher)
  :export (validate-modulators-pass))

(define (validate-modulators-pass programs output options)
  "Validate all modulators in a hans file"
  (define (parameter-exists? object name)
    (not (eq? #f (find (lambda (item)
                         (eq? item name))
                       (map parameter-name
                            (object-record-parameters
                              (hans-object-rec object)))))))

  (define (validate program)
    (for-each (lambda (modulator)
                (let ((src-object  (list-ref modulator 0))
                      (src-name    (list-ref modulator 1))
                      (src-comp    (list-ref modulator 2))
                      (dest-object (list-ref modulator 3))
                      (dest-name   (list-ref modulator 4))
                      (dest-comp   (list-ref modulator 5)))
                  ;; Self modulating
                  (if (and (eqv? (hans-object-instance-id src-object)
                                 (hans-object-instance-id dest-object))
                           (eqv? src-name dest-name)
                           (eqv? src-comp dest-comp))
                    (throw 'compileerror "Error: self modulating parameter"
                                         (hans-object-instance-id src-object)
                                         (object-record-name
                                           (hans-object-rec src-object))
                                         src-name))
                  ;; Check they exist
                  (if (not (parameter-exists? src-object src-name))
                    (throw 'compileerror "Error: Modulator parameter not found"
                                         (object-record-name
                                           (hans-object-rec src-object))
                                         src-name))
                  (if (not (parameter-exists? dest-object dest-name))
                    (throw 'compileerror "Error: Modulator parameter not found"
                                         (object-record-name
                                           (hans-object-rec dest-object))
                                         dest-name))))
              (hans-program-modulators program)))
  (for-each validate programs)
  output)
