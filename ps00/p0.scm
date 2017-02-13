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


      










