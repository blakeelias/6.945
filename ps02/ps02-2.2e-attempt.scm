(ge (make-top-level-environment))
(cd "/Users/blake/Dropbox\ (MIT)/Classes/6.945/ps02/code")
(load "load.scm")

;;; Problem 2.1 Warmup 

(define boolean-arithmetic
  (make-arithmetic 'boolean boolean? '()
		   (lambda (name)
		     (case name
		       ((additive-identity) #f)
		       ((multiplicative-identity) #t)
		       (else (default-object))))
  (lambda (operator)
    (let ((procedure
	   (case operator
	     ((+) (lambda (x y) (or x y)))
	     ((-) (lambda (x) (not x)))
	     ((*) (lambda (x y) (and x y)))
	     ((negate) (lambda (x) (not x)))
	     (else
	      (lambda args
		(error "Operator undefined in Boolean" operator))))))
      (and procedure
	   (simple-operation operator boolean? procedure))))))

(install-arithmetic! boolean-arithmetic)

; (install-arithmetic! symbolic-arithmetic-1)

;;; Tests of 2.1:


(+ #t #f)
;Value: #t

(* #t #t)
;Value: #t

(* #t #f)
;Value: #f

(- #t)
;Value: #f

(- #f)
;Value: #t


;;; Problem 2.2
; (a)

(ge (make-top-level-environment))
(cd "/Users/blake/Dropbox\ (MIT)/Classes/6.945/ps02/code")
(load "load.scm")

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

(define (v:*-scalar-vector scalar vec) ((vector-element-wise (lambda (x) (* scalar x))) vec))

(define (v:*-vector-scalar vec scalar) ((vector-element-wise (lambda (x) (* x scalar))) vec))

; (v:* -1 #(1 2 3))
;Value 184: #(-1 -2 -3)

(define (v:dot vector1 vector2)
  (ensure-vector-lengths-match (list vector1 vector2))
  (let ((product (lambda (l) (reduce * 1 l))))
    (sum
     (map product (zip (vector->list vector1) (vector->list vector2))))))

(define (v:*-maker base-domain?)
  (lambda (arg1 arg2)
    (cond
      ((and (vector? arg1) (base-domain? arg2)) (v:*-vector-scalar arg1 arg2))
      ((and (base-domain? arg1) (vector? arg2)) (v:*-scalar-vector arg1 arg2))
      ((and (vector? arg1) (vector? arg2)) (v:dot arg1 arg2)))))

(define (v:negate vec) (v:*-scalar-vector -1 vec))

(v:negate #(1 2 4))
;Value 193: #(-1 -2 -4)

(define (v:- vector1 vector2)
  (v:+ vector1 (v:negate vector2)))

(v:- #(1 2 5) #(3 4 6))
;Value 203: #(-2 -2 -1)

(define (v:magnitude vector)
  (sqrt (v:dot vector vector)))

(define (vector-extender base-arithmetic)
  (make-arithmetic
   'vector
   (disjoin vector? (arithmetic-domain-predicate base-arithmetic))
   (list base-arithmetic)
    (lambda (name base-constant)
      base-constant)
    (let ((base-predicate
	   (arithmetic-domain-predicate base-arithmetic)))
      (lambda (operator base-operation)
	(case operator
	  ((+)
	   (simple-operation
	    operator
	    vector?
	    (lambda (x y) (v:+ x y ))))
	  ((-)
	   (simple-operation
	    operator
	    vector?
	    (lambda (x y) (v:- x y))))
	  ((*)
	   (make-operation
	    operator
	    (any-arg (operator-arity operator)
		     vector?
		     base-predicate)
	    (v:*-maker base-predicate)))
	  ((negate) (lambda (x) (v:negate x)))
	  ((magnitude) (lambda (x) (v:magnitude x)))
	  (else
	   (lambda args
	     (error "Operator undefined in Vector" operator))))))))

(define vector-arithmetic
  (extend-arithmetic vector-extender numeric-arithmetic))

; This last line returns the following error:

;The object #[compound-procedure 856], passed as an argument to safe-cdr, is not a pair.


(install-arithmetic! vector-arithmetic)
