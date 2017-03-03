;;;; Simple predicate metadata

(define (register-predicate! predicate name)
  (set-predicate-metadata! predicate name)
  predicate)

(define (register-compound-predicate! predicate type components)
  (register-predicate! predicate
                       (cons type
                             (map predicate-name components))))

(define predicate-name get-predicate-metadata)
(define any-object? (conjoin))

(register-predicate! number? 'number)
(register-predicate! symbol? 'symbol)
(register-predicate! boolean? 'boolean)
