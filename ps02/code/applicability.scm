;;;; Applicability

;;; An applicability attribute is a list of lists, representing
;;; an OR of some per-argument ANDs.

(define (applicability? object)
  (and (list? object)
       (every (lambda (pattern)
                (and (list? pattern)
                     (every procedure? pattern)))
              object)
       (or (not (pair? object))
           (let ((arity (length (car object))))
             (every (lambda (pattern)
                      (n:= arity (length pattern)))
                    (cdr object))))))

(define (applicability-arity applicability)
  (guarantee applicability? applicability)
  (if (pair? applicability)
      (length (car applicability))
      0))

(define (is-applicable? applicability args)
  (and (n:= (length args) (applicability-arity applicability))
       (any (lambda (and-clause)
              (every (lambda (predicate arg)
                       (increment-predicate-count! predicate)
                       (predicate arg))
                     and-clause
                     args))
            applicability)))

(define (match-args . predicates)
  (list predicates))

(define (all-args arity predicate)
  (list (make-list arity predicate)))

(define (any-arg arity predicate base-predicate)
  (if (n:= 0 arity)
      (list)
      (let ((joint-predicate (disjoin base-predicate predicate)))
        (map (lambda (i)
               (map (lambda (j)
                      (if (n:= j i) predicate joint-predicate))
                    (iota arity)))
             (iota arity)))))

(define (applicability-union . applicabilities)
  (applicability-union* applicabilities))

(define (applicability-union* applicabilities)
  (apply lset-union equal? applicabilities))
