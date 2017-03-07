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


