;;;; Operation abstraction

(define (operation? object)
  (and (n:list? object)
       (n:= 4 (length object))
       (eq? 'operation (car object))
       (operator? (cadr object))
       (applicability? (caddr object))
       (procedure? (cadddr object))))

(define (make-operation operator applicability procedure)
  (list 'operation operator applicability procedure))

;;; API
(define (operation-applicability operation)
  (caddr operation))

;;; API
(define (operation-procedure operation)
  (cadddr operation))

;;; API
(define (apply-operation operation args)
  (apply (operation-procedure operation) args))

;;; API
(define (make-installable-operation-procedure procedure
                                              new-procedure)
  new-procedure)

;;; API
(define (operation-components operation)
  (list operation))

;;; API
(define (constant-union name . constants)
  (let ((unique
         (remove default-object?
                 (delete-duplicates constants eqv?))))
    (if (n:pair? unique)
        (car unique)
        (default-object))))

;;; API
(define (operation-union operator . operations)
  (operation-union* operator operations))

;;; API
(define (operation-union* operator operations)
  (make-operation operator
                  (applicability-union*
                   (map operation-applicability operations))
                  (lambda args
                    (operation-union-dispatch operator
                                              operations
                                              args))))

;; helper to make book description clearer
(define (operation-union-dispatch operator operations args)
  (let ((operation
         (find (lambda (operation)
                 (is-operation-applicable? operation args))
               operations)))
    (if (not operation)
        (error "Inapplicable operation:" operator args))
    (apply-operation operation args)))

;; helper to make book description clearer
(define (is-operation-applicable? operation args)
  (is-applicable? (operation-applicability operation) args))

;;; API
(define (simple-operation operator predicate procedure)
  (make-operation operator
                  (all-args (operator-arity operator)
                            predicate)
                  procedure))

;;; API
(define (simple-operation-procedure operation)
  (operation-procedure operation))