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

#|
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

|#


; (c)

#|
Problem 3:

`double-check-lock' ensures that a certain operation is performed only when appropriate given some state (determined by the prodecure `check'). The entire process of checking whether the condition is met, and then applying the appropriate action (`do' or `if-not'), needs to be performed atomically in order to ensure that no state-change takes place after the check but before the action, which could render the action in-valid if the state-change would make the check return a different value. 

This is relevant in set-variable-value! and define-variable! because both of those functions make state modifications to the environment (`env' variable) via `set-car!', when they determine that (car vals) is the appropriate value that needs to be overwritten with the new value. If, however, the state of the `env' variable changes in between the check and the re-writing, then it is possible that the re-writing could now be writing to the wrong place in `env'. So this needs to be wrapped up in a double-check-lock for safety.

|#


#|
Problem 4:

(a) What is meant by "doubly recursive"? If it means recursive on two arguments (as described here: https://en.wikipedia.org/wiki/Double_recursion), then I'm thinking of the memoized definition of `fib' that has two arguments (the last two numbers in the sequence) and is therefore able to act iteratively / do memoization, which is a reason why it would be faster.

If, on the other hand, double recursive means making these two recursive calls to fib, without any memoization, then this one is likely slower because it is doing this busy-wait while it waits for `x' and `y' to both be ready...

(b) My guess is that, the two recursive calls to `fib2' finish at roughly the same time, so `x' and `y' get set at about the same time. Then, the two recursive calls each check whether both x and y are set... and both will find that both variables are set! So both of them will call (c (+ x y)). This can be fixed by making the setting and checking happen together, atomically, as below, so that only one of them will find both variables as set (the second one will find this, and not the first). 

Code and tests below:

-----


(init)

eval> (define fib2
       (alpha (n c)
         (if (< n 2)
             (c n)
             (let ((x 'not-ready) (y 'not-ready))
               (define check-if-done
                 (lambda ()
                   (if (boolean/or (eq? x 'not-ready)
                                   (eq? y 'not-ready))
#f
                       (c (+ x y)))))
               (fib2 (- n 1)
                     (lambda (v)
                      (atomically (lambda ()
                       (set! x v)
                       (check-if-done)))))
               (fib2 (- n 2)
                     (lambda (v)
                      (atomically (lambda ()
                       (set! y v)
                       (check-if-done)))))))))
fib2

eval> (fib2 10 write-line)
actor-applied
55

eval> (fib2 10 write-line)
actor-applied
55

eval> (fib2 10 write-line)
actor-applied
55

eval> (fib2 10 write-line)
actor-applied
55

eval> (fib2 10 write-line)
actor-applied
55

eval> (fib2 10 write-line)
actor-applied
55

eval> (fib2 10 write-line)
actor-applied
55

eval> (fib2 10 write-line)
actor-applied
55

eval> (fib2 10 write-line)
actor-applied
55

eval> (fib2 10 write-line)
actor-applied
55

eval> (fib2 10 write-line)
actor-applied
55

eval> (fib2 10 write-line)
actor-applied
55

eval> (fib2 8 write-line)
actor-applied
21

eval> (fib2 8 write-line)
actor-applied
21

eval> (fib2 8 write-line)
actor-applied
21

eval> (fib2 8 write-line)
actor-applied
21

eval> (fib2 8 write-line)
actor-applied
21

eval> (fib2 8 write-line)
actor-applied
21

eval> (fib2 8 write-line)
actor-applied
21

eval> (fib2 8 write-line)
actor-applied
21

eval> (fib2 8 write-line)
actor-applied
21

eval> (fib2 8 write-line)
actor-applied
21

eval> (fib2 8 write-line)
actor-applied
21

eval> (fib2 12 write-line)
actor-applied
144

eval> (fib2 12 write-line)
actor-applied
144

eval> (fib2 12 write-line)
actor-applied
144

eval> (fib2 12 write-line)
actor-applied
144

eval> (fib2 12 write-line)
actor-applied
144

eval> (fib2 12 write-line)
actor-applied
144

eval> (fib2 12 write-line)
actor-applied
144

eval> (fib2 9 write-line)
actor-applied
34

eval> (fib2 9 write-line)
actor-applied
34

eval> (fib2 9 write-line)
actor-applied
34

eval> (fib2 9 write-line)
actor-applied
34

eval> (fib2 9 write-line)
actor-applied
34

eval> (fib2 9 write-line)
actor-applied
34



The above results are pretty promising: in 36 tests, 0 had multiple prints. Whereas with the original version of fib2, in 9 testss, 2 did multiple-prints (shown below). (I used the same numerical argument to fib2, as well.) Stop telling us things are "very hard" when they're quite simple to solve!!! (At least, I think I've solved this one...)


eval> (fib2 10 write-line)
actor-applied
55

eval> (fib2 10 write-line)
actor-applied
55

eval> (fib2 10 write-line)
actor-applied
55

eval> (fib2 10 write-line)
actor-applied
55

eval> (fib2 8 write-line)
actor-applied
21

eval> (fib2 8 write-line)
actor-applied
21
21

eval> (fib2 14 write-line)
actor-applied
377

eval> (fib2 14 write-line)
actor-applied
377

eval> (fib2 14 write-line)
actor-applied
377
377


|#


#| Problem 5 |#

;(a)

(define-record-type
    future
    (make-future cont done? value)
    future?
  (cont   continuation)
  (done?  done?     set-done!)
  (value  get-value set-value!))

;(b)
#|
(define (future cont)
  (make-future cont #f 'not-ready))

(define (wait future cont)
  (cont ((continuation future))))
|#

#|
  (if (done? future)
      ;(add-to-tasks!
      ; actor
       (lambda ()
	 (cont (get-value future)))
      ;)
      #f))
|#