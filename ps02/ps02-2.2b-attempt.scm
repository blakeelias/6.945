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

(define (v:+-maker +)
  (let ((sum (lambda (l) (+ 0 l))))
    (lambda (vector1 vector2)
      (ensure-vector-lengths-match (list vector1 vector2))
      (list->vector
       (map sum (zip (vector->list vector1) (vector->list vector2)))))))

(define (v:* scalar vec) ((vector-element-wise (lambda (x) (n:* x scalar))) vec))

(define (v:negate vec) (v:* -1 vec))

(define (v:--maker +)
  (let ((v:+ (v:+-maker +)))
    (lambda (vector1 vector2)
      (v:+ vector1 (v:negate vector2)))))

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
	  (let ((plus (operation-procedure (arithmetic-operation '+ base-arithmetic))))
	    (case operator
	      ((+) (v:+-maker plus))
	      ((-) (v:--maker plus))
	      ; ((*) (lambda (x y) (vec-dot x y)))
	      ((negate) (lambda (x) (v:negate x)))
	      (else
	       (lambda args
		 (error "Operator undefined in Vector" operator))))))))))

(define vector-arithmetic
  (extend-arithmetic vector-extender numeric-arithmetic))

(install-arithmetic! vector-arithmetic)

;(+ #(1 2) #(3 4))
;Value 366: #(4 6)

;(+ 1 2)
;Value: 3

;(- #(1 2 3) #(4 5 6))
;Value 239: #(-3 -3 -3)

;(- #(1 2 3))
;Value 240: #(-1 -2 -3)

(define symbolic-vector-arithmetic 
  (extend-arithmetic symbolic-extender vector-arithmetic))

(install-arithmetic! symbolic-vector-arithmetic)

;; Tests of symbolic vector arithmetic:

(+ 1 2)
;Value: 3

(+ #(1 2) #(3 4))
;Value 420: #(4 6)

(- #(1 2 3))
;Value 421: #(-1 -2 -3)

(- #(1 2 3) #(4 5 6))
;Value 422: #(-3 -3 -3)

(+ 'a 'b)
;Value 423: (+ a b)

(+ 'a 1)
;Value 424: (+ a 1)

(+ #(1 2 3) #(4 b 6))
;Value 427: #(5 (+ b 2) 9)






