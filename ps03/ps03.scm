					

(ge (make-top-level-environment))
(cd "/Users/blake/Dropbox\ (MIT)/Classes/6.945/ps03/code")
(load "load.scm")

;;; Vector Extender (from ps02)

(register-predicate! vector? 'vector)

(define (ensure-vector-lengths-match vecs)
  ( let (( first-vec-length ( vector-length ( car vecs ))))
    ( if ( any ( lambda ( v)
		 ( not (n:= ( vector-length v)
			  first-vec-length )))
	       vecs)
	 (error "Vector dimension mismatch:" vecs))))

(define (vector-element-wise element-procedure)
  (lambda vecs
    (ensure-vector-lengths-match vecs)
    (apply vector-map element-procedure vecs)))

((vector-element-wise (lambda (x) (n:* x 2))) #(1 2 3))
;Value 153: #(2 4 6)

(define (sum l) (reduce + 0 l))

(define (v:+ vector1 vector2)
  (ensure-vector-lengths-match (list vector1 vector2))
  (list->vector
   (map sum (zip (vector->list vector1) (vector->list vector2)))))

(v:+ #(1 2 3) #(4 5 6))
;Value 181: #(5 7 9)

(define (v:* scalar vec) ((vector-element-wise (lambda (x) (n:* x scalar))) vec))

(v:* -1 #(1 2 3))
;Value 184: #(-1 -2 -3)

(define (v:negate vec) (v:* -1 vec))

(v:negate #(1 2 4))
;Value 193: #(-1 -2 -4)

(define (v:- vector1 vector2)
  (v:+ vector1 (v:negate vector2)))

(v:- #(1 2 5) #(3 4 6))
;Value 203: #(-2 -2 -1)

(define (v:dot vector1 vector2)
  (ensure-vector-lengths-match (list vector1 vector2))
  (let ((product (lambda (l) (reduce * 1 l))))
    (sum
     (map product (zip (vector->list vector1) (vector->list vector2))))))

(define (v:magnitude vector)
  (sqrt (v:dot vector vector)))

(define (vector-extender base-arithmetic)
  (make-arithmetic 'vector vector? (list base-arithmetic)
    (lambda (name base-constant)
      base-constant)
    (let ((base-predicate
	   (arithmetic-domain-predicate base-arithmetic)))
      (lambda (operator base-operation)
	(simple-operation
	  operator
	  vector?
	  (case operator
	    ((+) (lambda (x y) (v:+ x y)))
	    ((-) (lambda (x y) (v:- x y)))
	    ((*) (lambda (x y) (v:dot x y)))
	    ((negate) (lambda (x) (v:negate x)))
	    ((magnitude) (lambda (x) (v:magnitude x)))
	    (else
	     (lambda args
	       (error "Operator undefined in Vector" operator)))))))))



;;; Problem 3.1


#|
3.1 (b)
The generic system is able to support both expressions, using the code below:
|#

#|
(let ((g (make-generic-arithmetic simple-generic-dispatcher)))
  (add-to-generic-arithmetic! g numeric-arithmetic)
  (extend-generic-arithmetic! g symbolic-extender)
  (extend-generic-arithmetic! g function-extender)
  (extend-generic-arithmetic! g vector-extender)
  (install-arithmetic! g))
a
(define (unit-circle x)
  (vector (sin x) (cos x)))
|#

#|
Tests:

((magnitude unit-circle) 'a)
;Value 22: (sqrt (+ (* (cos a) (cos a)) (* (sin a) (sin a))))

((magnitude (vector sin cos)) 'a)
;Value 23: (sqrt (+ (* (cos a) (cos a)) (* (sin a) (sin a))))
|#

; 3.1 (c)

;;; Here is one way to do something like this:
#|
(define (n:vector . args) (apply vector args))

(register-predicate! list? 'list)

(define vector-new (simple-generic-procedure 'vector-new 2))
(define-generic-procedure-handler vector-new
  (all-args 2 (disjoin number? symbol? list?))
  (lambda (a b) (n:vector a b)))
(define-generic-procedure-handler vector-new
  (all-args 2 function?)
  (lambda (a b) (lambda (x) (vector-new (a x) (b x)))))
|#

#|
Tests:

; Vectors of numbers:
(vector-new 1 2)
;Value 22: #(1 2)

; Vectors of functions:
((vector-new sin cos) 'a)
;Value 34: #((sin a) (cos a))

((vector-new cos sin) 3)
;Value 25: #(-.9899924966004454 .1411200080598672)


; We can even have vectors of (functions that return functions)!
 
(define cos-mult
  (lambda (a)
    (lambda (x)
      (cos (* a x)))))

(define sin-mult
  (lambda (a)
    (lambda (x)
      (sin (* a x)))))


(((vector-new cos-mult sin-mult) 2) 'a)
;Value 66: #((cos (* 2 a)) (sin (* 2 a)))

; Can still take dot products because this new vector type still returns a vector
(* (vector-new 2 3) (vector-new 1 2))
;Value: 8

; Can take dot product of this new vector type as well, even when it's still left in functional form:
((* (vector-new cos sin) (vector-new cos sin)) 'a)
;Value 76: (+ (* (sin a) (sin a)) (* (cos a) (cos a)))


; Can even compute the magnitude of it:

(magnitude (vector-new cos sin))
;Value 77: #[compound-procedure 77]

((magnitude (vector-new cos sin)) 'a)
;Value 78: (sqrt (+ (* (sin a) (sin a)) (* (cos a) (cos a))))


This does have a couple of shortcomings:
1) We could not use the function name "vector" - had to give it the new name "vector-new". Trying to overwrite the name "vector" would cause all calls to "vector" to hang indefinitely and never produce a result.
2) This only supports fixed-length vectors - in this case, vectors of length 2 - because we had to define "vector-new" as a generic procedure taking a certain number of arguments, rather than before where we just had generic procedures which take vectors as arguments (where those vectors could be of arbitrary length).
|#


; 3.2
#|
(display "In problem 3.2")

(display (+))
(display (*))
(display (+ (+)))
(display (* (*)))
(display (* (+)))
(display (+ (*)))

(display "new arithmetic")

(define (symbolic-extender base-arithmetic)
  (make-arithmetic 'symbolic symbolic? (list base-arithmetic)
    (lambda (name base-constant)
      (default-object))
    (let ((base-predicate
           (arithmetic-domain-predicate base-arithmetic)))
      (lambda (operator base-operation)
        (make-operation operator
                        (any-arg (operator-arity operator)
                                 symbolic?
                                 base-predicate)
                        (lambda args (cons operator args)))))))
						
(define g (make-generic-arithmetic simple-generic-dispatcher))
(add-to-generic-arithmetic! g (symbolic-extender numeric-arithmetic))
(install-arithmetic! g)

(display "new arithmetic tests")

(display (+))
(display (*))
(display (+ (+)))
(display (* (*)))
(display (* (+)))
(display (+ (*)))
(display (* 'a 'b))
|#

; 3.5

; (a)

; No, there is still the same dependence on order -- the last extender that is applied is the one that takes priority. 

; Try it in one order:
#|
(ge (make-top-level-environment))
(cd "/Users/blake/Dropbox\ (MIT)/Classes/6.945/ps03/code")
(load "load.scm")

(define trie-full-generic-arithmetic
  (let ((g (make-generic-arithmetic trie-generic-dispatcher)))
    (add-to-generic-arithmetic! g numeric-arithmetic)
    (extend-generic-arithmetic! g symbolic-extender)
    (extend-generic-arithmetic! g function-extender)
    g))
(install-arithmetic! trie-full-generic-arithmetic)
|#

; It works!
#|
(+ 'a ((+ 'c cos sin) (* 2 'b)))
;Value 137: (+ a (+ (+ c (cos (* 2 b))) (sin (* 2 b))))
|#


; Try the other order!
#|
(ge (make-top-level-environment))
(cd "/Users/blake/Dropbox\ (MIT)/Classes/6.945/ps03/code")
(load "load.scm")


(define trie-full-generic-arithmetic
  (let ((g (make-generic-arithmetic trie-generic-dispatcher)))
    (add-to-generic-arithmetic! g numeric-arithmetic)
    (extend-generic-arithmetic! g function-extender)
    (extend-generic-arithmetic! g symbolic-extender)
    g))
(install-arithmetic! trie-full-generic-arithmetic)
|#

; Doesn't work:
#|
(+ 'a ((+ 'c cos sin) (* 2 'b)))
;The object (+ (+ c #[compound-procedure 81]) #[compound-procedure 82]) is not applicable.
;To continue, call RESTART with an option number:
; (RESTART 8) => Specify a procedure to use in its place.
; (RESTART 7) => Return to read-eval-print level 7.
; (RESTART 6) => Return to read-eval-print level 6.
; (RESTART 5) => Return to read-eval-print level 5.
; (RESTART 4) => Return to read-eval-print level 4.
; (RESTART 3) => Return to read-eval-print level 3.
; (RESTART 2) => Return to read-eval-print level 2.
; (RESTART 1) => Return to read-eval-print level 1.
;Start debugger? (y or n): 

|#

; 3.5 (b)
#|
There can be more than one appropriate handler for a sequence of arguments if there is an argument that satisfies two different predicates, with each predicate satisfying a different handler. For instance, '(1 2) satisfies both symbolic? and list?, each of which may have its own handler.

(symbolic? '(1 2))
;Value: #t

(list? '(1 2))
;Value: #t
|#

; 3.5 (c)
#|
In the generic arithmetic code we have written so far, there are no such situations. The predicates symbolic?, vector?, number? and function? are mutually exclusive. However, if one defined some new argument predicates which had overlap with these existing ones (eg. (or vector? number?), which will have overlap with vector? in terms of which arguments it returns #t for), then there can be more than one handler for a sequence of arguments.
|#


; 3.6

#|
There may be two predicates that are mathematically the same function (i.e. will gave the same outputs for the same inputs), but in Scheme will not be seen as the same procedure, if they are produced from a procedure like conjoin or disjoin. This will cause there to be separate branches in the trie representing these two "different" predicates, thus turning one branch into two (and causing everything downstream of that branch to have a duplicate, I think, in the other branch - really, duplicating a sub-trie and not just a branch). 

By memoizing disjoin and conjoin, we can make sure that these mathematically identical functions will also be returned as the same procedure, so that they will be seen as eqv? by Scheme.
|#

#|
(define disjoin-copy disjoin)
(define disjoin (memoize-multi-arg-eqv disjoin-copy))

(define conjoin-copy conjoin)
(define conjoin (memoize-multi-arg-eqv conjoin-copy))
|#

#|
(eqv? (disjoin symbolic? number?) (disjoin symbolic? number?))
;Value: #t

(eqv? (conjoin symbolic? number?) (conjoin symbolic? number?))
;Value: #t
|#


; 3.7
#|
(ge (make-top-level-environment))
(cd "/Users/blake/Dropbox\ (MIT)/Classes/6.945/ps03/code")
(load "load.scm")

(define full-generic-arithmetic
  (let ((g (make-generic-arithmetic simple-generic-dispatcher)))
    (add-to-generic-arithmetic! g numeric-arithmetic)
    (extend-generic-arithmetic! g function-extender)
    (add-to-generic-arithmetic! g 
				(symbolic-extender numeric-arithmetic))
    g))

(define trie-full-generic-arithmetic
  (let ((g (make-generic-arithmetic trie-generic-dispatcher)))
    (add-to-generic-arithmetic! g numeric-arithmetic)
    (add-to-generic-arithmetic! g
      (symbolic-extender numeric-arithmetic))
    (extend-generic-arithmetic! g function-extender)
    g))

 (install-arithmetic! full-generic-arithmetic)

(define (fib n)
  (if (< n 2)
      n
      (+ (fib (- n 1)) (fib (- n 2)))))

; (install-arithmetic! trie-full-generic-arithmetic)

; (with-predicate-counts ( lambda () ( fib 20) ))

;Value 565: #[package 565 (uninstall (generic))]
|#

#|
with trie-full-generic-arithmetic:

(10946 (disjoin any-object function))
(10946 (disjoin number symbolic))
(21892 (disjoin any-object function))
(21891 (disjoin any-object function))
(109453 function)
(109453 symbolic)
(109453 number)
(21891 (disjoin number symbolic))
(21892 (disjoin number symbolic))
;Value: 6765
|#


#|

with full-generic-arithmetic

(21892 (disjoin any-object function))
(10946 (disjoin any-object function))
(109453 symbolic)
(109453 function)
(109453 number)
(10946 (disjoin number symbolic))
(21892 (disjoin number symbolic))
(21891 (disjoin number symbolic))
(21891 (disjoin any-object function))
;Value: 6765

|#


#|

With the list-of-predicate-lists approach, it is possible that one of the first few predicate lists that gets tried actually matches the arguments, and so the program quickly arrives at the correct matching of arguments to predicates. Whereas with trie-lookup, you have to test the first argument against every predicate at the first level of the trie, to find all the ones that match. And then from those that match, you have to explore every possible branch coming off of those first-level nodes. So tries help us avoid redundant calls, but potentially still do some calls which are extraneous.

On the other hand, if there are many rules which share a common pre-fix, then tries will be more efficient. Because if the arguments *don't* match that prefix of predicates, then the predicates in that sub-trie will not have to be explored at all. Whereas in the rule-based version, all those irrelevant rules would still have to be tested one-by-one.

|#






; Problem 4.0


(define (generic-dispatcher)
  (cached-generic-dispatcher implementation-type-name))

(define (cached-generic-dispatcher get-key)
  (make-cached-generic-dispatcher (simple-generic-dispatcher)
				  get-key))

(define (make-cached-generic-dispatcher base-dispatcher get-key)
  (let ((get-handler
	 (simple-list-memoizer eqv?
			       hash-by-eqv
			       (lambda (args) (map get-key args))
			       (base-dispatcher 'get-handler))))
    (lambda (message)
      (case message
	((get-handler) get-handler)
	(else (base-dispatcher message))))))

(let ((g (make-generic-arithmetic generic-dispatcher)))
  (add-to-generic-arithmetic! g numeric-arithmetic)
  (extend-generic-arithmetic! g symbolic-extender)
  (extend-generic-arithmetic! g function-extender)
  (extend-generic-arithmetic! g vector-extender)
  (install-arithmetic! g))


(define (fib n)
  (if (< n 2)
      n
      (+ (fib (- n 1)) (fib (- n 2)))))

(with-predicate-counts ( lambda () ( fib 20) ))

