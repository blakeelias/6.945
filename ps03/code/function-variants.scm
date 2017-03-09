;;;; Alternative function extenders

(define (pure-function-extender codomain-arithmetic)
  (make-arithmetic 'pure-function
                   function?
                   (list codomain-arithmetic)
    (lambda (name codomain-constant)
      (lambda args
        codomain-constant))
    (lambda (operator codomain-operation)
      (simple-operation operator function?
        (lambda functions
          (lambda args
            (apply-operation codomain-operation
                             (map (lambda (function)
                                    (apply function args))
                                  functions))))))))

(define (function-extender-with-coercion codomain-arithmetic)
  (let ((codomain-predicate
         (arithmetic-domain-predicate codomain-arithmetic)))
    (make-arithmetic 'function-with-coercion
                     (disjoin codomain-predicate function?)
                     (list
                      (function-extender codomain-arithmetic))
      (lambda (name function-constant)
        function-constant)
      (lambda (operator function-operation)
        (make-operation operator
                        (any-arg (operator-arity operator)
                                 function?
                                 codomain-predicate)
          (lambda things
            (apply-operation function-operation
                             (map (lambda (thing)
                                    (if (function? thing)
                                        thing
                                        (lambda args thing)))
                                  things))))))))
