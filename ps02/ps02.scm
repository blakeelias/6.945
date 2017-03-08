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

(define (v:* scalar vec) ((vector-element-wise (lambda (x) (n:* x scalar))) vec))

(v:* -1 #(1 2 3))
;Value 184: #(-1 -2 -3)

(define (v:negate vec) (v:* -1 vec))

(v:negate #(1 2 4))
;Value 193: #(-1 -2 -4)



#|
(define vector-arithmetic
  (make-arithmetic 'vector vector? '()
		   (lambda (name)
		     (case name
		       ((additive-identity) 0)
		       ((multiplicative-identity) 1)
		       (else (default-object))))
  (lambda (operator)
    (let ((procedure
	   (case operator
	     ((+) (lambda (x y) (vec-add x y)))
	     ((-) (lambda (x y) (vec-sub x y)))
	     ((*) (lambda (x y) (vec-dot x y)))
	     ((negate) (lambda (x) (vec-mult x -1)))
	     (else
	      (lambda args
		(error "Operator undefined in Boolean" operator))))))
      (and procedure
	   (simple-operation operator boolean? procedure))))))

(install-arithmetic! vector-arithmetic)
|#
