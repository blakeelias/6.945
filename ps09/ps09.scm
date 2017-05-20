;(cd "/Users/blake/Dropbox (MIT)/Classes/6.945/ps09/code/")
(load "/Users/blake/Dropbox (MIT)/Classes/6.945/ps09/code/load")

#| 

Problem 1

(define (foo n)
 (define (buzz m)
 (if (not (= m 0)) (buzz (- m 1))))
 (define iter
 (alpha (l i)
 (if (not (= i 0))
 (begin
 (if (eq? l 'a) (buzz (* 100 i)) (buzz (* 100 (- n i))))
 (pp (list l i))
 (iter l (- i 1))))))
 (iter 'a n)
 (iter 'b n))

The reason we get this output order is that the early calls to `iter' with 'a as the first argument and with i as 10, take a long time to complete; whereas the `iter' calls with 'b as the first argument and with i as 10 complete faster. So a few 'b calls can finish before the first 'a one does, and a few more 'b calls can finish before the next 'a one does, etc.  

Doesn't seem so strange to me....

|#

#|

Problem 2

(a) I wouldn't expect this to have much difference. If evaluating the operands is going to take a long time, and the operands are themselves applications of alpha expressions (as opposed to simple calls to primitive procedures). If this is the case, then evaluating the operands will reduce to evaluating the body of a procedure, which will go into the same queueing system. 

But I suppose it can't hurt to try something like this....

|#

; (b)

(defhandler apply
  (lambda (actor operands calling-environment)
    (if (not (= (length (actor-parameters actor))
		(length operands)))
	(error "Wrong number of operands supplied"))
    (let ((arguments
	   (map (lambda (parameter operand)
		  (add-to-tasks! actor
				 (lambda ()
				   (evaluate-procedure-operand parameter
							       operand
							       calling-environment)))
		  'actor-applied)
		(actor-parameters actor)
		operands)))
      (add-to-tasks! actor
		     (lambda ()
		       (eval (actor-body actor)
			     (extend-environment
			      (map procedure-parameter-name
				   (actor-parameters actor))
			      arguments
			      (actor-environment actor)))))
      'actor-applied))
  actor-procedure?)




; (c)

#|
Problem 3:

`double-check-lock' ensures that a certain operation is performed only when appropriate given some state (determined by the prodecure `check'). The entire process of checking whether the condition is met, and then applying the appropriate action (`do' or `if-not'), needs to be performed atomically in order to ensure that no state-change takes place after the check but before the action, which could render the action in-valid if the state-change would make the check return a different value. 

This is relevant in set-variable-value! and define-variable! because both of those functions make state modifications to the environment (`env' variable) via `set-car!', when they determine that (car vals) is the appropriate value that needs to be overwritten with the new value. If, however, the state of the `env' variable changes in between the check and the re-writing, then it is possible that the re-writing could now be writing to the wrong place in `env'. So this needs to be wrapped up in a double-check-lock for safety.

|#


#|
Problem 4:

(a) This is slow because it does not do any memoization, while a doubly-recursive function would. 


|#
