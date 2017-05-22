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

eval> (f 2 3) 
6

; This last line shows that we can still call procedures that are defined and get the correct answer, and not just get back (f 2 3):

|#


#|

Problem 4:

(a): KONS seems pretty good to me...

(b) On executing that line, I get the following error:

eval> (ref-stream (solve (lambda (x) x) 1 0.001) 1000)

;Unbound variable: dy
;To continue, call RESTART with an option number:
; (RESTART 4) => Specify a value to use instead of dy.
; (RESTART 3) => Define dy to a given value.
; (RESTART 2) => Return to read-eval-print level 2.
; (RESTART 1) => Return to read-eval-print level 1.
;Start debugger? (y or n): n

The problem is that `y' is defined to make use of `dy', but `dy' is defined in terms of `y'.

This is fix-able by making all the stream arguments lazy - so that the stream itself does not have to be defined until its values need to be read. See below.


(init)

eval>  (define (add-streams s1 s2)
 (kons (+ (car s1) (car s2))
 (add-streams (cdr s1) (cdr s2))))
add-streams

eval>  (define (ref-stream stream n)
 (if (= n 0)
 (car stream)
 (ref-stream (cdr stream) (- n 1))))
ref-stream

eval> (define (map-stream proc (items lazy))
 (kons (proc (car items))
 (map-stream proc (cdr items))))
map-stream

eval> (define (scale-stream (items lazy) factor)
 (map-stream (lambda (x) (* x factor))
 items))
scale-stream

eval> (define (integral (integrand lazy) initial-value dt)
 (define int
 (kons initial-value
 (add-streams (scale-stream integrand dt)
 int)))
 int)
integral

eval> (define (solve f y0 dt)
 (define y (integral dy y0 dt))
 (define dy (map-stream f y))
 y)
solve

eval> (ref-stream (solve (lambda (x) x) 1 0.001) 10)
1.0100451202102523

|#

#|

Problem 5:

(a) Some computations might have state which gets updated, in which case memoizing could be dangerous. The Fibonacci numbers is a pure mathematical function of its argument, but other data might not have this property. 

(b) Well, let's see...

eval> (define (kons2 (a lazy memo) (d lazy memo)) (cons a d))
kons2

eval> (kons2 1 2)
(1 . 2)

Uh-oh... that looks the same as cons...

eval> (cons 1 2)
(1 . 2)

Whereas kons would have done something different:

eval> (kons 1 2)
(#(delayed-memo 1 #((solve integral scale-stream map-stream ref-stream add-streams allow-self-evaluating-symbols) (#[compound-procedure 68] #[compound-procedure 69] #[compound-procedure 70] #[compound-procedure 71] #[compound-procedure 72] #[compound-procedure 73] #f) (*the-empty-environment*))) . #(delayed-memo 2 #((solve integral scale-stream map-stream ref-stream add-streams allow-self-evaluating-symbols) (#[compound-procedure 68] #[compound-procedure 69] #[compound-procedure 70] #[compound-procedure 71] #[compound-procedure 72] #[compound-procedure 73] #f) (*the-empty-environment*))))

The problem is that lazy arguments only remain un-evaluated up until the point that they are needed for a computation that uses them. In the case of the given definition of kons, with `a' and `d' both lazy, both arguments immediately get used by being passed to `cons', and so both need to be evaluated right away, making them not so lazy...

(c) The expressions inside the `kons' pair are susceptible to state changes. One example of something which could go wrong (depending what our understand of "wrong" is, in the particular situation...):

eval> (define x 3)
x

eval> (cons x x)
(3 . 3)

eval> (define a (kons x x))
a

eval> (car a)
3

eval> (set! x 4)
#!unspecific

eval> (car a)
3

eval> (cdr a)
4

Here, we see that when the value of `x' changes, the value of any un-evaluated parts of a `kons' pair change - but the value of the previously evaluated parts does not. So, when `x' had initially been defined as 3 and then stored in (kons x x), when we went to retrieve a value from the pair later, we may be surprised that the value is 4. We may also be surprised that an object which was constructed as (kons x x) now has different values for its `car' and its `cdr'!

|#

#| 

Problem 6




|#
