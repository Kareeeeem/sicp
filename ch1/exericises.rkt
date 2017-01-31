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
; ---

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
; ---

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
; ---

; Exercise 1.4
; if b is larger than 0 the operation is PLUS otherwise MINUS.
; this is equivalent to (+ a (abs b))
(define (a-plus-abs-b a b)
  ((if (> b 0) + -) a b))
; ---

; Exercise 1.5
(define (p) (p))
(define (test x y)
  (if (= x 0) 0 y))

; (test 0 (p))
; Applicative-order will first evaluate the arguments meaning it would attempt
; to evaluate (p) which will keep evaluating to (p) indefinitely causing an
; infinite loop. Normal-order will only evaluate when the value is needed.
; In this case the value is never needed so (test 0 (p)) evaluates to 0.
; ---

; Exercise 1.6
; With new-if the interpreter will always evaluate all it's arguments, and thus
; will keep calling sqrt-iter causing an infinite loop.
; Newtons method for improving the guess of nth root of x.
; ---

; Exercise 1.7
(define (abs x)
  (cond ((> x 0) x)
        ((= x 0) 0)
        ((< x 0) (- x))))

(define (pow x y)
  (cond ((= y 0) 1)
        ((< y 0) (/ 1 (pow x (abs y))))
        (else (* x (pow x (dec y))))))

(define (nroot n)
  (lambda (x)
    (define (good-enough? n guess)
      (<= (abs (- (pow guess n) x)) 0.001))
    (define (good-enough2? n guess)
      (<=
        (abs (- (improve-quess n guess) guess))
         (* guess .001)))
    (define (improve-quess n guess)
      (/ (+ (/ x (pow guess (dec n)))
            (* (dec n) guess))
         n))
    (define (root-iter n guess)
      (cond ((= x 0) 0)
            ((good-enough2? n guess) guess)
            (else (root-iter n (improve-quess n guess)))))
    (root-iter n 1.0)))

(define sqroot (nroot 2))

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
; ---

; Exercise 1.8
; See the previous exercise for the definition of nroot. I've tried to make it
; general but perhaps this is not the right way.
; ---

; Exercise 1.9
; (define (+ a b)
;   (if (= a 0) b (inc (+ (dec a) b))))
; (+ 4 5)
; (inc (+ 3 5))
; (inc (inc (+ 2 5))
; (int (inc (inc (+ 1 5))))
; (inc (inc (inc (inc (+ 0 5)))))
; (inc (inc (inc (inc 5))))
; (inc (inc (inc 6)))
; (inc (inc 7))
; (inc 8)
; 9
; (define (+ a b)
;   (if (= a 0) b (+ (dec a) (inc b))))
; (+ 4 5)
; (+ 3 6)
; (+ 2 7)
; (+ 1 8)
; (+ 0 9)
; 9
; ---

; Exercise 1.10
(define (A x y)
  (cond ((= y 0) 0)
        ((= x 0) (* 2 y))
        ((= y 1) 2)
        (else (A (- x 1) (A x (- y 1))))))

; (A 1 10) -> 1024
; (A 2 4) -> 65536
; (A 3 3) -> 65536

(define (f n) (A 0 n)) ; 2n
(define (g n) (A 1 n)) ; 2n when n > 0, 0 when n = 0
(define (h n) (A 2 n)) ; 2^h(n-1) when n > 1, 2 when n == 1, 0 when n == 0

; Exercise 1.11
; This is pretty much the exact same thing as fibonachi.
; Recursive
(define (f-recursive n)
  (if (< n 3)
    3
    (+ (f (- n 1))
       (f (- n 2))
       (f (- n 3)))))

; Iterative
(define (f-iterative n)
  (iter 3 3 3 n)

  (define (iter a b c test)
    (if (< test 3)
      c
      (iter b c (+ a b c) (- test 1)))))
; ---

; Exercise 1.12
(define (pascal row col)
  (cond ((or (< col 1) (< row col)) #f)
        ((or (= col 1) (= col row)) 1)
        (else (+ (pascal (- row 1) col)
                 (pascal (- row 1) (- col 1))))))
; ---
