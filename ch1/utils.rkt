#lang sicp

(define (square x)
  (* x x))

(define (sum-of-squares x y)
  (+ (square x) (square y)))

; greater than or equal
(define (>= a b)
  (not (< a b)))

(define (abs x)
  (cond ((> x 0) x)
        ((= x 0) 0)
        ((< x 0) (- x))))

(define (pow x y)
  (cond ((= y 0) 1)
        ((< y 0) (/ 1 (pow x (abs y))))
        (else (* x (pow x (dec y))))))

; Newtons method for improving the guess of nth root of x.
(define (improve-quess n guess x)
  (/ (+
       (/ x (pow guess (dec n)))
       (* (dec n) guess))
     n))

(define (good-enough? n guess x)
  (<= (abs (- (improve-quess n guess x) guess))
     (* guess .001)))

(define (root-iter n guess x)
  (cond ((= x 0) 0)
        ((good-enough? n guess x) guess)
        (else (root-iter n (improve-quess n guess x) x))))

(define (nroot n)
  (lambda (x) (root-iter n 1.0 x)))
