#lang sicp

; Exercise 1.1
10 ; 10
(+ 5 3 4) ; 12
(- 9 1) ; 8
(/ 6 2) ; 3
(+ (* 2 4) (- 4 6)) ; 6
(define a 3) ; -
(define b (+ a 1)) ; -
(+ a b (* a b)) ; 19
(if (and (> b a) (< b (* a b)))
  b
  a) ; 4
(cond ((= a 4) 6)
      ((= b 4) (+ 6 7 a))
      (else 25)) ; 16
(+ 2 (if (> b a) b a)) ; 6
(* (cond ((> a b) a)
         ((< a b) b)
         (else -1))
   (+ a 1)) ; 16

; Exercise 1.2
(/
  (+ 5
     4
     (- 2
        (- 3
           (+ 6
              (/ 4 5)))))
  (* 3
     (- 6 2)
     (- 2 7))
  )
; Exercise 1.3
(define (square x)
  (* x x))

(define (sum-of-squares x y)
  (+ (square x) (square y)))

(define (>= a b)
  (not (< a b)))

(define (sum-squares-two-largest a b c)
  (if (>= a b)
    (sum-of-squares a (if (>= b c) b c))
    (sum-of-squares b (if (>= a c) a c))))

; Exercise 1.4
; if b is larger than 0 the operation is PLUS otherwise MINUS.
; this is equivalent to (+ a (abs b))
(define (a-plus-abs-b a b)
  ((if (> b 0) + -) a b))

; Exercise 1.5
(define (p) (p))
(define (test x y)
  (if (= x 0) 0 y))

; (test 0 (p))
; Applicative-order will first evaluate the arguments meaning it would attempt
; to evaluate (p) which will keep evaluating to (p) indefinitely causing an
; infinite loop. Normal-order will only evaluate when the value is needed.
; In this case the value is never needed so (test 0 (p)) evaluates to 0.

; Exercise 1.6
; With new-if the interpreter will always evaluate all it's arguments, and thus
; will keep calling sqrt-iter causing an infinite loop.
; Newtons method for improving the guess of nth root of x.

; Exercise 1.7
(define (abs x)
  (cond ((> x 0) x)
        ((= x 0) 0)
        ((< x 0) (- x))))

(define (pow x y)
  (cond ((= y 0) 1)
        ((< y 0) (/ 1 (pow x (abs y))))
        (else (* x (pow x (dec y))))))

(define (improve-quess n guess x)
  (/ (+
       (/ x (pow guess (dec n)))
       (* (dec n) guess))
     n))

(define (good-enough? n guess x)
  (<=
    (abs (- (pow guess n) x))
    0.001))

(define (good-enough2? n guess x)
  (<=
    (abs (- (improve-quess n guess x) guess))
     (* guess .001)))

(define (root-iter n guess x)
  (cond ((= x 0) 0)
        ((good-enough? n guess x) guess)
        (else (root-iter n (improve-quess n guess x) x))))

(define (nroot n)
  (lambda (x) (root-iter n 1.0 x)))

(define (sqroot x)
  ((nroot 2) x))

(define (test-sqroot x)
  (abs (- x (square (sqroot x)))))

; for the following values the good enough2 version
; (abs (- value (square (sqroot value))))
; is much smaller
;
; value       good-enough            good-enough2
; 0.01        6.526315785885042e-05  1.0579156517996624e-07
; 0.00000001  0.0009765591601630759  8.122264028019588e-14
; 0.000002    0.0009758948042280276  7.442005618853216e-11
; 0.0001      0.0009438358335233747  1.4281284086209629e-08
;
; 9827349234  9827250101.012384      9828251899.472069
; 123         111.90946353527008     111.91388871229876
; 10000       9900.00005072658       9901.420988021504
; 10e+20      infinite loop          6.716504326841303e+17

; So for smaller values the second approach is much better. For bigger numbers
; it seems good-enough2? results in less accurate results.  However
; good-enough? would result in an infinite loop with very large numbers,
; good-enough2? does not. This is because the computer cannot accurately
; represent small differences between very large numbers.

; Exercise 1.8
; See the previous exercise for the definition of nroot. I've tried to make it
; general but perhaps this is not the right way.
