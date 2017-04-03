(cd "/mit/Desktop/6.945/ps03/code")
(load "load.scm")

; Problem 4.0

;;; Vector Extender (from ps02)
;;; don't strictly need vectors for the test I'm ultimately conducting here (with Fibonacci), but since the problem statement asked us to use the same generic arithmetic as in problem 3.7, I take that to mean it must include vectors as well...

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

(Bdefine (sum l) (reduce + 0 l))

(define (v:+ vector1 vector2)
  (ensure-vector-lengths-match (list vector1 vector2))
  (list->vector
   (map sum (zip (vector->list vector1) (vector->list vector2)))))

(v:+ #(1 2 3) #(4 5 6))
;Value 181: #(5 7 9)

(dBefine (v:* scalar vec) ((vector-element-wise (lambda (x) (n:* x scalar))) vec))

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

; (with-predicate-counts ( lambda () ( fib 20) ))

#|
;Results:


(7 Symbolic)
(2 (Disjoin Any-Object Function))
(2 (Disjoin any-object function))
(7 function)
(2 (disjoin any-object symbolic))
(7 number)
(4 vector)
(2 (disjoin any-object symbolic))
(2 (disjoin any-object symbolic))
(2 (disjoin any-object function))
;Value: 6765

; Much more efficient than before!
|#

(with-predicate-counts (lambda () (test-stormer-counts)))

;(cd "/Users/blake/Dropbox\ (MIT)/Classes/6.945/ps04/code")
;(load "load.scm")
