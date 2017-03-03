;;;; Symbolic arithmetic

(define (symbolic? object)
  (or (symbol? object)
      (pair? object)))
(register-predicate! symbolic? 'symbolic)

(define (symbolic-extender base-arithmetic)
  (make-arithmetic 'symbolic symbolic? (list base-arithmetic)
    (lambda (name base-constant)
      base-constant)
    (let ((base-predicate
           (arithmetic-domain-predicate base-arithmetic)))
      (lambda (operator base-operation)
        (make-operation operator
                        (any-arg (operator-arity operator)
                                 symbolic?
                                 base-predicate)
                        (lambda args (cons operator args)))))))

;;;; Function arithmetic

(define function? procedure?)
(register-predicate! function? 'function)

(define (function-extender codomain-arithmetic)
  (let ((codomain-predicate
         (arithmetic-domain-predicate codomain-arithmetic)))
    (make-arithmetic 'function
                     (disjoin codomain-predicate function?)
                     (list codomain-arithmetic)
      (lambda (name codomain-constant)
        codomain-constant)
      (lambda (operator codomain-operation)
        (make-operation operator
                        (any-arg (operator-arity operator)
                                 function?
                                 codomain-predicate)
          (lambda things
            (lambda args
              (apply-operation codomain-operation
                               (map (lambda (thing)
                                      (if (function? thing)
                                          (apply thing args)
                                          thing))
                                    things)))))))))

;;;; Book examples

(define (make-arithmetic-1 name get-operation)
  (make-arithmetic name any-object? '()
    (lambda (name)
      (case name
        ((additive-identity) 0)
        ((multiplicative-identity) 1)
        (else (default-object))))
    (lambda (operator)
      (simple-operation operator
                        any-object?
                        (get-operation operator)))))

(define symbolic-arithmetic-1
  (make-arithmetic-1 'symbolic
    (lambda (operator)
      (lambda args (cons operator args)))))
