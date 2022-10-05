#lang sicp

; exercise 1.16

(define (even? x)
  (= (remainder x 2) 0))

; realize that the accumulator, does not *have* to be modified each iteration
; if n is even, we just modify b and n in such a way that the result does not actually
; change, and so a should not change either ab^n should stay constant
(define (ex-iter b n a)
  (cond ((= n 0) a)
        ((even? n)
         (ex-iter (* b b) (/ n 2) a))
        (else
          (ex-iter b (- n 1) (* a b)))))

(define (ex b n)
  (ex-iter b n 1))

; exercise 1.17

(define (double x) (+ x x))
(define (halve x) (/ x 2))

(define (fast-mul x y)
  (cond ((= y 0) 0)
        ((even? y) (double (fast-mul x (halve y))))
        (else (+ x (fast-mul x (- y 1))))))


; exercise 1.18

(define (mul-iter x y a)
  (cond ((= y 0) a)
        ((even? y)
         (mul-iter (double x) (halve y) a))
        (else
          (mul-iter x (- y 1) (+ a x)))))

(define (mul x y)
  (mul-iter x y 0))
