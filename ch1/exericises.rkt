#lang sicp

; EXERCISE 1.1
; ============
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

; EXERCISE 1.2
; ============
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

; EXERCISE 1.3
; ============
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

; EXERCISE 1.4
; ============
; if b is larger than 0 the operation is PLUS otherwise MINUS.
; this is equivalent to (+ a (abs b))
(define (a-plus-abs-b a b)
  ((if (> b 0) + -) a b))

; EXERCISE 1.5
; ============
(define (p) (p))
(define (test x y)
  (if (= x 0) 0 y))

; (test 0 (p))
; Applicative-order will first evaluate the arguments meaning it would attempt
; to evaluate (p) which will keep evaluating to (p) indefinitely causing an
; infinite loop. Normal-order will only evaluate when the value is needed.
; In this case the value is never needed so (test 0 (p)) evaluates to 0.

; EXERCISE 1.6
; ============
; With new-if the interpreter will always evaluate all it's arguments, and thus
; will keep calling sqrt-iter causing an infinite loop.
; Newtons method for improving the guess of nth root of x.

; EXERCISE 1.7
; ============
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

(define sqrt (nroot 2))

(define (test-sqrt x)
  (abs (- x (square (sqrt x)))))

; for the following values the good enough2 version
; (abs (- value (square (sqrt value))))
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

; EXERCISE 1.8
; ============
; See the previous exercise for the definition of nroot. I've tried to make it
; general but perhaps this is not the right way.

; EXERCISE 1.9
; ============
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

; EXERCISE 1.10
; =============
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

; EXERCISE 1.11
; =============
; This is pretty much the exact same thing as fibonachi.
(define (f-recursive n)
  (if (< n 3)
    3
    (+ (f (- n 1))
       (f (- n 2))
       (f (- n 3)))))

(define (f-iterative n)
  (define (iter a b c test)
    (if (< test 3)
      c
      (iter b c (+ a b c) (dec test))))
  (iter 3 3 3 n))


; EXERCISE 1.12
; =============
(define (pascal row col)
  (cond ((or (< col 1) (< row col)) #f)
        ((or (= col 1) (= col row)) 1)
        (else (+ (pascal (dec row) col)
                 (pascal (dec row) (dec col))))))

; EXERCISE 1.13
; =============
; https://en.wikipedia.org/wiki/Mathematical_induction
(define (fib n)
  (define (iter a b count)
    (if (= count 0)
      b
      (iter (+ a b) a (- count 1))))
  (iter 1 0 n))

(define sqrt5 (sqrt 5))
(define psi (/ (+ 1 sqrt5) 2))
(define phi (/ (- 1 sqrt5) 2))

; 1. The basis (base case): prove that the statement holds for the first
; natural number n. Usually, n = 0 or n = 1, rarely, n = –1 (although not a
; natural number, the extension of the natural numbers to –1 is still a
; well-ordered set).

; 2. The inductive step: prove that, if the statement holds for some natural
; number n, then the statement holds for n + 1.

(define (prove f n)
  (and
    (f 0)         ; Base case, n = 0
    (f n)         ; n, for any natutal number
    (f (+ n 1)))) ; Inductive step, n + 1

(define (test-fib-equals n)
  (= (fib n)
     (/ (- (pow psi n)
           (pow phi n))
        sqrt5)))
(prove test-fib-equals 5)

; For the following I read the following for reference. I am not familiar
; with mathematical proofs.
; http://www.billthelizard.com/2009/12/sicp-exercise-113-fibonacci-and-golden.html
;
; Now prove that (/ (pow psi n) sqrt5) is infact the closest integer to (fib n)
; We can rewrite the internals for test-fib-equals follows
(define (test-fib-equals2 n)
  (< (- (- (fib n)
           (/ (pow psi n) sqrt5))
        (* -1 (/ (pow phi n) sqrt5))) ; The difference
     0.000001))                       ; because precision is not perfect.
(prove test-fib-equals2 5)

; To prove that (fib n) is the closest integer to (/ (pow psi n) sqrt5) we have
; to prove that (/ (pow phi n) sqrt5) is always smaller than (/ 1 2).

(define (test-diff-less-than-half n)
  (< (/ (pow phi n) sqrt5)
     (/ 1 2)))
(prove test-diff-less-than-half 5)

(define (test-diff-less-than-half2 n)
  (< (pow phi n)
     (/ sqrt5 2))) ; 1.118...
(prove test-diff-less-than-half2 5)

; phi is -0.618304...

(define (phi-to-nth-less-or_equal n)
  (<= (pow phi n) 1))
(prove phi-to-nth-less-or_equal 5)

; (pow phi n) will never exceed 1 for postive n. And (< 1 (/ sqrt5 2))
; Thus (fib n) is the closest integer to (/ (pow psi n) sqrt5)
