(ge (make-top-level-environment))

(load "load")

(define pure-numeric-function-arithmetic
  (extend-arithmetic pure-function-extender
		     numeric-arithmetic))

(install-arithmetic! pure-numeric-function-arithmetic)

((+ cos sin) 3)
;Value: -.8488724885405782

(* 2 ((+ cos sin) 3))
;Value: -1.6977449770811563

;(* 2 ((+ 1 cos sin) 3))
;Error

(define mixed-numeric-function-arithmetic
 (extend-arithmetic function-extender
		    numeric-arithmetic))

(install-arithmetic! mixed-numeric-function-arithmetic)

(* 2 ((+ 1 cos sin) 3))
;Value: .3022550229188436


(define (literal-function name)
  (lambda args
    (cons name args)))

(define combined-arithmetic
  (extend-arithmetic symbolic-extender
                     numeric-arithmetic))

(define full-arithmetic
  (extend-arithmetic function-extender-with-coercion
		     combined-arithmetic))


(install-arithmetic! full-arithmetic)

(* 'b ((+ 4 cos sin) (+ 3 'a)))
;Value: (* b (+ (+ 4 (cos (+ 3 a))) (sin (+ 3 a))))

(+ 'a ((+ cos sin) 'b))
;Value: (+ a (+ (cos b) (sin b)))

(+ 'a ((+ cos sin) 3))
;Value: (+ a -.8488724885405782)

(+ 'a ((+ 3 cos sin) 'b))
;Value: (+ a (+ (+ 3 (cos b)) (sin b)))

(+ 'a ((+ 'c cos sin) 'b))
;Value: (+ a (+ (+ c (cos b)) (sin b)))

(+ 'a ((+ (literal-function 'c) cos sin) 'b))
;Value: (+ a (+ (+ (c b) (cos b)) (sin b)))


;;; Here are some simple examples.

(install-arithmetic! numeric-arithmetic)

;;; x'' = -x
(define (F t x) (- x))

(define s0
  (make-initial-history 0 .01 (sin 0) (sin -.01) (sin -.02)))

(x 0 ((evolver F .01 stormer-2) s0 100))
;Value: .8414709493275624

(define combined-arithmetic
  (extend-arithmetic symbolic-extender
                     numeric-arithmetic))

(install-arithmetic! combined-arithmetic)

(+)
;Value: 0

(+ 1 2 3)
;Value: 6

(+ 1 'a 3)
;Value: (+ (+ 1 a) 3)

(x 0
   ((evolver F .01 stormer-2)
    s0 100))
;Value: .8414709493275624

(pp (x 0
       ((evolver F 'h stormer-2)
        (make-initial-history 't 'h 'xt 'xt-h 'xt-2h)
        1)))
#|
(+ (+ (* 2 xt)
      (* -1 xt-h))
   (* (/ (expt h 2) 12)
      (+ (+ (* 13 (negate xt))
            (* -2 (negate xt-h)))
         (negate xt-2h))))
|#

(pp (x 0
       ((evolver F 'h stormer-2)
        (make-initial-history 't 'h 4 'xt-h 'xt-2h)
        1)))
#|
(+ (+ 8 (* -1 xt-h))
   (* (/ (expt h 2) 12)
      (+ (+ -52 (* -2 (negate xt-h)))
         (negate xt-2h))))
|#

(pp (x 0
       ((evolver F 'h stormer-2)
        s0
        1)))
#|
(+ 9.999833334166664e-3
   (* (/ (expt h 2) 12)
      -9.999750002487318e-7))
|#

