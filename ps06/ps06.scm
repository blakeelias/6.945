(load "/Users/blake/Dropbox (MIT)/Classes/6.945/ps06/code/load")

#|
 Problem 1
|#

#| Code:

(defhandler apply
  (lambda (procedure operands calling-environment)
	(list->vector
	  (map (lambda (proc) (apply proc operands calling-environment))
	    (vector->list procedure))))
  vector?)

; Tests:

(init)

eval> ((vector cos sin) 0.6)
#(.8253356149096783 .5646424733950354)

eval> (cos 0.6)
.8253356149096783

|#

#|
 Problem 3
|#

(defhandler eval
  (lambda (expression environment)
    (call-with-current-continuation
     (lambda (cont)
       (bind-condition-handler
        '()
        (lambda (e)
          (cont expression)) ;; self-evaluate
        (lambda () (lookup-variable-value expression environment))))))
  variable?)

(define (disjoin . predicates)
  (disjoin* predicates))

(define (disjoin* predicates)
;  (guarantee-list-of predicate? predicates)
  (lambda (object)
    (any (lambda (predicate)
	   (predicate object))
	 predicates)))


(define n:* *)
(define n:/ /)
(define n:+ +)
(define n:- -)

(define * (make-generic-operator 2 '* n:*))
(defhandler * (lambda (x y) `(* ,x ,y)) (disjoin variable? list?) number?)
(defhandler * (lambda (x y) `(* ,x ,y)) number? (disjoin variable? list?))
(defhandler * (lambda (x y) `(* ,x ,y)) (disjoin variable? list?) (disjoin variable? list?))

(define / (make-generic-operator 2 '/ n:/))
(defhandler / (lambda (x y) `(/ ,x ,y)) (disjoin variable? list?) number?)
(defhandler / (lambda (x y) `(/ ,x ,y)) number? (disjoin variable? list?))
(defhandler / (lambda (x y) `(/ ,x ,y)) (disjoin variable? list?) (disjoin variable? list?))

(define + (make-generic-operator 2 '+ n:+))
(defhandler + (lambda (x y) `(+ ,x ,y)) (disjoin variable? list?) number?)
(defhandler + (lambda (x y) `(+ ,x ,y)) number? (disjoin variable? list?))
(defhandler + (lambda (x y) `(+ ,x ,y)) (disjoin variable? list?) (disjoin variable? list?))

(define - (make-generic-operator 2 '- n:-))
(defhandler - (lambda (x y) `(- ,x ,y)) (disjoin variable? list?) number?)
(defhandler - (lambda (x y) `(- ,x ,y)) number? (disjoin variable? list?))
(defhandler - (lambda (x y) `(- ,x ,y)) (disjoin variable? list?) (disjoin variable? list?))



; 3 (b)

(defhandler apply
  (lambda (procedure-name operands calling-environment)
    (cons procedure-name operands))
  symbol?)

#|
  Tests:

(init)

eval> (f 1 2)
(f 1 2)

eval> (+ (f 3) (* 4 5))
(+ (f 3) 20)
|#

;; Now, to allow these extensions only if the user sets the value ALLOW-SELF-EVALUATING-SYMBOLS to #t:

(define (init)
  (set! the-global-environment
  (extend-environment '(ALLOW-SELF-EVALUATING-SYMBOLS) '(#f) the-empty-environment))
  (repl))
