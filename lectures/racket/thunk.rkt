; Don't compute value unless we need it
; If we need it, remember the value for future reference
; Called "Lazy evaluation"
#lang racket

(provide (all-defined-out))

(define (my-mult x y-thunk)
    (cond [(= x 0) 0]
          [(= x 1) (y-thunk)]
          [#t (+ (y-thunk) (my-mult (- x  1) y-thunk))]))

(define (my-delay th)
    (mcons #f th))

; (false, _) false indicates an unset force
; (true, x)  true indicates set force
; to use, call my-force with thunk you want to evaluate
(define (my-force p)
    (if (mcar p)
        (mcdr p)
        (begin (set-mcar! p #t)
               (set-mcdr! p ((mcdr p)))
               (mcdr p))))
(my-mult 10 (let ([p (my-delay (lambda() (+ 3 4)))])
             (lambda () (my-force p))))
            