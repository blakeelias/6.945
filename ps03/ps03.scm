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

(let ((g (make-generic-arithmetic simple-generic-dispatcher)))
  (add-to-generic-arithmetic! g numeric-arithmetic)
  (extend-generic-arithmetic! g symbolic-extender)
  (extend-generic-arithmetic! g function-extender)
  (extend-generic-arithmetic! g vector-extender)
  (install-arithmetic! g))

(define (unit-circle x)
  (vector (sin x) (cos x)))

#|
Tests:

((magnitude unit-circle) 'a)
;Value 22: (sqrt (+ (* (cos a) (cos a)) (* (sin a) (sin a))))

((magnitude (vector sin cos)) 'a)
;Value 23: (sqrt (+ (* (cos a) (cos a)) (* (sin a) (sin a))))
|#

; 3.1 (c)

(define (n:vector . args) (apply vector args))

(define vector-new (simple-generic-procedure 'vector-new 2))
(define-generic-procedure-handler vector-new
  (all-args 2 number?)
  (lambda (a b) (n:vector a b)))
(define-generic-procedure-handler vector-new
  (all-args 2 function?)
  (lambda (a b) (lambda (x) (n:vector (a x) (b x)))))



#|
Tests:

((vector-new sin cos) 'a)
;Value 34: #((sin a) (cos a))


((vector-new sin cos) 'a)
;Value 34: #((sin a) (cos a))
|#