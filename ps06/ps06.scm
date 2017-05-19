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
 Problem 2

This does not work because built-in procedures that take procedural arguments, such as map, were defined under the built-in eval/apply definitions. In the built-in "apply" procedure, compound procedures that we pass in from our new interpreter don't appear to be applicable, because our compound procedure record type is different from the underlying one, and the compound-procedure? predicate in our interpreter is different from the underlying one. 

A strategy for fixing this would be to get our new interpreter to use the same compound-procedure record type as the underlying Scheme.

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

(defhandler eval
  (lambda (expression environment)
    (if (not (lookup-variable-value 'ALLOW-SELF-EVALUATING-SYMBOLS environment))
	(lookup-variable-value expression environment)
	(call-with-current-continuation
	 (lambda (cont)
	   (bind-condition-handler
	       '()
	       (lambda (e)
		 (cont 
		  expression)) ;; catch error
	     ;; self-evaluate
	     (lambda ()
	       (lookup-variable-value expression environment) ;; "try"
	       ))))))
  variable?)

(defhandler apply
  (lambda (procedure-name operands calling-environment)
    (if (not (lookup-variable-value 'ALLOW-SELF-EVALUATING-SYMBOLS calling-environment))
	(error (string-append "Unbound variable:" (symbol->string procedure-name)))
	(cons procedure-name operands)))
  symbol?)


#| Tests:


(init)

eval> (+ (* 2 3) (* 4 5))
26

eval> (+ (* a 3) (* 4 5))

;Unbound variable: a
;To continue, call RESTART with an option number:
; (RESTART 10) => Specify a value to use instead of a.
; (RESTART 9) => Define a to a given value.
; (RESTART 8) => Return to read-eval-print level 8.
; (RESTART 7) => Return to read-eval-print level 7.
; (RESTART 6) => Return to read-eval-print level 6.
; (RESTART 5) => Return to read-eval-print level 5.
; (RESTART 4) => Return to read-eval-print level 4.
; (RESTART 3) => Return to read-eval-print level 3.
; (RESTART 2) => Return to read-eval-print level 2.
; (RESTART 1) => Return to read-eval-print level 1.
;Start debugger? (y or n): n

(go)

eval> (f 1 2)

;Unbound variable: f
;To continue, call RESTART with an option number:
; (RESTART 11) => Specify a value to use instead of f.
; (RESTART 10) => Define f to a given value.
; (RESTART 9) => Return to read-eval-print level 9.
; (RESTART 8) => Return to read-eval-print level 8.
; (RESTART 7) => Return to read-eval-print level 7.
; (RESTART 6) => Return to read-eval-print level 6.
; (RESTART 5) => Return to read-eval-print level 5.
; (RESTART 4) => Return to read-eval-print level 4.
; (RESTART 3) => Return to read-eval-print level 3.
; (RESTART 2) => Return to read-eval-print level 2.
; (RESTART 1) => Return to read-eval-print level 1.
;Start debugger? (y or n): n

(go)

eval> (set! ALLOW-SELF-EVALUATING-SYMBOLS #t)
#!unspecific

eval> (+ (* a 3) (* 4 5))
(+ (* a 3) 20)

eval> (f 1 2)
(f 1 2)

eval> (define (f x y) (* x y))
f

eval> (f 2 3) ; Can still call procedures that are defined, and not just get back (f 2 3):
6

|#

