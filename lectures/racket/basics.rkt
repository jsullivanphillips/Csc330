#lang racket

(provide (all-defined-out))

(define x 3)
(+ x 2)

(define fun1
    (lambda (x)
        (* x (* x x))))

(fun1 3)

(define (pow1 x y)
    (if (= y 0)
        1
        (* x (pow1 x (- y 1)))))

(pow1 2 3)
(pow1 3 0)

;currying
(define pow2
    (lambda (x)
        (lambda (y)
            (pow1 x y))))

(define three-to-the (pow2 3))

(three-to-the 2)
((pow2 4) 2) ;because (pow2 4) returns a function

;currying example done