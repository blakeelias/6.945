;;;; Blake Elias
;;;; 6.945 Problem Set 0
;;;; February 17, 2017

(load "~/Dropbox (MIT)/Classes/6.945/ps00/p0utils.scm")

#|
Problem 1
|#

; Question (a)

(modulo 13 8)
;Value: 5

(remainder 13 8)
;Value: 5

(modulo -13 8)
;Value: 3

(remainder -13 8)
;Value: -5

(modulo -13 -8)
;Value: -5

(remainder -13 -8)
;Value: -5

; Question (b)

#|
(modulo x n) returns an answer in the range [0, n - 1] for n > 0, and an answer in the range [n + 1, 0] for n < 0

(remainder x n) returns an answer in the range [0, sign(x)*(n - 1)] for n > 0 (i.e. (remainder -13 8) ==> -5).

`modulo' is the better choice for implementing modular arithmetic as described in the problem statement, as that system is restricted to n > 0, and always must return a nonnegative answer, which `modulo' indeed does when n > 0.
|#

; Question (c)
(define +mod
  (lambda (a b n)
    (modulo (+ a b) n)))

(define -mod
  (lambda (a b n)
    (modulo (- a b) n)))

(define *mod
  (lambda (a b n)
    (modulo (* a b) n)))

(+mod 7 5 8)
;Value: 4

(+mod 10 10 3)
;Value: 2

(-mod 5 12 2)
;Value: 1

(*mod 6 6 9)
;Value: 0

(+mod 99 99 100)
;Value: 98

(*mod 50 -3 100)
; Volue: 50


; Question (d)

(define modular
  (lambda (modulus op)
    (lambda (a1 a2)
      (modulo (op a1 a2) modulus))))

((modular 17 +) 13 11) ; Value: 7

((modular 17 -) 13 11) ; Value: 2
((modular 17 *) 13 11) ; Value: 7


;;;; 


#| 
Problem 2
|#

; Question (a)
#|
The order of growth in time is O(b).
The order of growth in space is O(b).
`slow-exptmod' uses an iterative algorithm.
|#

; Question (b)

(define (exptmod p)
  (let ((mod* (modular p *)))
    (define (square x)
      (mod* x x))
    (define (em base exponent)
      (cond ((= exponent 0) 1)
	    ((= exponent 1) (modulo base p))
	    (else (mod* (em (square base) (quotient exponent 2))
		     (em base (remainder exponent 2))))))
    em))

; a^b = (a^2)^(floor(b/2)) * a^(b % 2)

((exptmod 10) 2 0)
;Value: 1

((exptmod 10) 2 3)
;Value: 8

((exptmod 10) 3 4)
;Value: 1

((exptmod 100) 2 15)
;Value: 68

((exptmod 100) -5 3)
;Value: 75

; Question (c)
#|
Order of growth in time: (O (log b))
Order of growth in space: (O (log b))
This is a recursive algorithm.
|#

; Problem 3

; Question (a)
(define (random-k-digit-number k)
  (define (append-random-digits n k)
    (if (= k 0) 
	n
	(append-random-digits (+ (* n 10) (random 10))
			      (- k 1))))
  (append-random-digits 0 k))


(random-k-digit-number 1)
;Value: 4

(random-k-digit-number 3)
;Value: 854

(random-k-digit-number 3)
;Value: 217

(random-k-digit-number 50)
;Value: 82705443447165714375013121540307676222994333132619

(random-k-digit-number 0)
;Value: 0

(random-k-digit-number 0)
;Value: 0

(random-k-digit-number -1)
; does not return... procedure only specified to take an integer k > 0, so I'm not going to worry about this case.

; Question (b)

(define (count-digits n)
  (define (count-digits-iter n d)
    (if (= n 0)
	d
	(count-digits-iter (quotient n 10) (+ d 1))))
  (count-digits-iter n 0))


(count-digits 1)
;Value: 1

(count-digits 3)
;Value: 1

(count-digits 2007)
;Value: 4

(count-digits 123456789)
;Value: 9

; Question (c)

(define (big-random n)
  (define (check r)
    (if (< r n)
	r
	(big-random n)))
  (check (random-k-digit-number (count-digits n))))


(big-random 100)
;Value: 2

(big-random 100)
;Value: 94

(big-random 1)
;Value: 0

(big-random 1)
;Value: 0

(big-random 1)
;Value: 0

(big-random (expt 10 40))
;Value: 8382525344980642626468971168699813023589

(big-random 500)
;Value: 56

(big-random 500)
;Value: 59

(big-random 500)
;Value: 234

(big-random 500)
;Value: 482


; Problem 4

#|
(a) Order of growth in time: (O n)  [by this I mean what would commonly be written as O(n), but I want to use prefix notation here!]. This is because the program has to check each integer k, where 2 <= k <= n, and determine if k is a factor of n. Assuming that (remainder k n) is a constant-time operation with respect to k and n (which is probably not quite true, but likely to be small, perhaps logarithmic in n), then we can see that `remainder' will be called up to n times.
    Order of growth in space: (O 1).
       This is because the process evolves as follows:
        (slow-prime? 5)
        (test-factors 5 2)
        (test-factors 5 3)
        (test-factors 5 4)
        (test-factors 5 5)
        #t
 
      The process just replaces k each time with (+ k 1), so there is no need for extra space to be used other than that used to make the original call to `test-factors'.

     slow-prime? uses an iterative algorithm.

(b) Ben's suggestion of only checking factors less than or equal to (sqrt n) would make the order of growth in time be (O (sqrt n)).

(c) Checking only odd factors and 2 would cut the execution time in half. In terms of order of growth, this would not make any difference. For (O (/ n 2)) is the same as (O n), and (O ((sqrt n) 2)) is the same as (O (sqrt n)).

|#

; (d) Test Fermat's Little Theorem:

(define (from-a-to-b a b)
  (cond
   ((< a b) (cons a (from-a-to-b (+ a 1) b)))
   ((= a b) (list b))
   (else '())))

(define (fermat-check-all-cases p)
  (let ((expt-p (exptmod p)))
    (map (lambda (a)
	   (= a
	      (expt-p a p)))
	 (from-a-to-b 0 (- p 1)))))


(fermat-check-all-cases 5)
;Value 18: (#t #t #t #t #t)

(fermat-check-all-cases 17)
;Value 19: (#t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t)

; Fermat's little theorem seems to hold when p is prime.

(fermat-check-all-cases 6)
;Value 20: (#t #t #f #t #t #f)

; And it doesn't hold in this case when p = 6 (a composite number).


; (e)

(define prime-test-iterations 20)

(define prime?-iter
      (lambda (p n-iter)
	(cond
	 ((< p 2) #f)
	 ((<= n-iter 0) #t)
	 (else
	    (let ((a (big-random (- p 1))))
	      (if (= ((exptmod p) a p) a)
		  (prime?-iter p (- n-iter 1))
		  #f))))))
	    

(define prime?
  (lambda (p)
    (prime?-iter p 20)))


(prime? 1)
;Value: #f

(prime? 0)
;Value: #f

(prime? 2)
;Value: #t

(prime? 4)
;Value: #f

(prime? 200)
;Value: #f

(prime? 199)
;Value: #t

(prime? -1)
;Value: #f

(prime? -7)
;Value: #f

;; Don't care about negative numbers; they can't be prime by definition.

; (f) Order of growth in time: 
; 20 tests of p satisfying Fermat's little theorem.
; Each test involves:
;  Randomly generating a value for a < p
;    counting the digits of `p': (O (log p))
;    Generating a random number of that many digits, and testing that it's less than p: (O (log p))
;  Testing the value for `a' (check whether a^p = a (mod p)). Exponentiate by repeated squaring: (O (log p)).
; Thus the total running time is (O (* 20 (log p))), or (O (log p)).
; Quite a speed-up from the (O (sqrt p)) order of growth in time with Ben Bitdittle's proposals!

; Order of growth in space: (O (log p)), which provides bits to record the value of p, which may be large.

; This procedure uses an iterative algorithm.


; Problem 5

(define random-k-digit-prime
  (lambda (k)
    (let ((n (random-k-digit-number k)))
      (if (prime? n)
	  n
	  (random-k-digit-prime k)))))

; Types of failure:
;   - getting unlucky and either taking a long time or never returning at all, due to the random numbers generated repeatedly being composite
;   - returning a composite number but reporting it to be prime (a Carmichael Number)

(random-k-digit-prime 2)
;Value: 5

(random-k-digit-prime 3)
;Value: 839

;Value: 2

(random-k-digit-prime 1)
;Value: 2

(random-k-digit-prime 2)
;Value: 7

(random-k-digit-prime 10)
;Value: 8967067379

(count-digits (random-k-digit-prime 100))
;Value: 100

(count-digits (random-k-digit-prime 100))
;Value: 100

(count-digits (random-k-digit-prime 100))
;Value: 100

(count-digits (random-k-digit-prime 100))
;Value: 100

(count-digits (random-k-digit-prime 100))
;Value: 100


; Problem 6

(define ax+by=1
  (lambda (a b)
    (let ((q (quotient a b))
	  (r (remainder a b)))
      (if (= r 1)
	  (list r (- q))
	  (let ((sol (ax+by=1 b r)))
	    (list (cadr sol)
		  (- (car sol) (* q (cadr sol)))))))))

; Tests


(ax+by=1 17 13)
;Value 31: (-3 4)

(ax+by=1 7 3)
;Value 32: (1 -2)

(ax+by=1 10 27)
;Value 33: (-8 3)

(define (inversemod n)
  (lambda (e)
    (if (not (= (gcd e n) 1))
	(error "inversemod requires (= (gcd e n) 1)")
	; find d such that ed = 1 (mod n)
	; means ed + nk = 1 for some int. k
	(modulo (car (ax+by=1 e n)) n))))


((inversemod 11) 5)
;Value: 9

((inversemod 11) 9)
;Value: 5

((inversemod 11) 7)
;Value: 8

((inversemod 12) 5)
;Value: 5

((inversemod 12) 8)
;inversemod requires (= (gcd e n) 1)
;To continue, call RESTART with an option number:
; (RESTART 8) => Return to read-eval-print level 8.
; (RESTART 7) => Return to read-eval-print level 7.
; (RESTART 6) => Return to read-eval-print level 6.
; (RESTART 5) => Return to read-eval-print level 5.
; (RESTART 4) => Return to read-eval-print level 4.
; (RESTART 3) => Return to read-eval-print level 3.
; (RESTART 2) => Return to read-eval-print level 2.
; (RESTART 1) => Return to read-eval-print level 1.
;Start debugger? (y or n): n

(random-k-digit-prime 2)
;Value: 61

((inversemod 101) 61)
;Value: 53

(*mod 53 61 101)
;Value: 1


; Problem 7

(define (eg-send-message message receiver)
  (let ((public-key (eg-receiver-public-key receiver))
	(decryption-procedure (eg-receiver-decryption-procedure receiver)))
    (let ((dh-system (eg-public-key-system public-key))
	  (receiver-advertised-number (eg-public-key-number public-key)))
      (let ((p (dh-system-prime dh-system))
	    (a (dh-system-primitive-root dh-system))
	    (k (dh-system-size dh-system)))
	(let ((my-secret (random-k-digit-number k))
	      (mod-expt (exptmod p))
	      (mod-* (modular p *)))
	  (decryption-procedure
	   (eg-make-ciphertext (mod-expt a my-secret) 
			       (mod-* (string->integer message)
				      (mod-expt
				       receiver-advertised-number
				       my-secret)))))))))

(define dh-system (public-dh-system 100))
(define Alyssa (eg-receiver dh-system))

(eg-send-message "Hi there." Alyssa)
;Value 40: "Hi there."

(eg-send-message "Hi there, Alyssa. 1234567890 0123456789" Alyssa)
;Value 46: "Hi there, Alyssa. 1234567890 0123456789"

(eg-send-message "Hi there, Alyssa. 1234567890 0123456789 00" Alyssa)
;Value 47: "¬Q\016à­c\017²Ô$$\022´²ÀAÊ\210à°Æ}\210°P\nªDÍ|ð°\024x_Ã\213´\223/_"

(eg-send-message "Hi there, Alyssa. 1234567890 0123456789 0" Alyssa)
;Value 48: "Hi there, Alyssa. 1234567890 0123456789 0"
;;; This is the longest string I can send that will be correctly decrypted with a 100 digit system. It is indeed not too long!


; Problem 8

(define Eve-Alyssa (Eve Alyssa))

(define (Eve-il receiver)
  (let ((receiver-public-key
	 (eg-receiver-public-key receiver))
	(receiver-decryption-procedure
	 (eg-receiver-decryption-procedure receiver)))
    (let ((dh-system (eg-public-key-system receiver-public-key)))
      (let ((my-receiver (eg-receiver dh-system)))
	(let ((my-public-key (eg-receiver-public-key my-receiver))
	      (my-decryption-procedure
	       (eg-receiver-decryption-procedure my-receiver)))
	  (let ((my-spying-procedure
		 (lambda (ciphertext)
		   (let ((message (my-decryption-procedure ciphertext)))
		     (write message)
		     (newline)
		     (eg-send-message (string-append message ", good friend ;)") receiver))))) 
	    (eg-make-receiver my-public-key
			      my-spying-procedure)))))))

(define Eve-il-Alyssa (Eve-il Alyssa))


(eg-send-message "Hi there." Eve-il-Alyssa)
"Hi there."
;Value 57: "Hi there., good friend ;)"

;; I have defined Eve-il to be the evil version of Eve.
;; (The first string is the original string that was sent, which Eve-il can now print out.
;; The second string, the returned value, is the original message with something else appended by Eve-il (", good friend ;)").
;; The nasty trick is that Eve-il makes her own receiver to wrap around Alice's,
;; presenting her own public key to Ben. 
;; Eve-il's decryption procedure proceeds to decrypt Ben's message, print it out,
;; then re-encrypt it (with some extra stuff appended) and send it to Alyssa,
;; who is none-the-wiser to this.
