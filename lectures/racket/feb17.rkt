#lang racket

(provide (all-defined-out))

(define (desc n)
  (if (= n 0)
      (list 0)
      (cons n (desc (- n 1)))))

(define (asc n)
  (define (help i)
    (if (= i n)
        (list n)
        (cons i (help (+ i 1)))))
  (help 0))


(define p (mcons 1 2))
(define q p)
(println q)
(println p)
(set! p (mcons 10 1))
(set-mcdr! q 0)
(println q)
(println p)

(define lst
  (list (cons 1 2) (cons 3 4) (cons 5 (cons 6 null))))
(assoc 3 lst) ;returns '(3 . 4) i.e. pair
(assoc 5 lst) ;returns '(5 6)   i.e. list