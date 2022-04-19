#lang racket

(provide (all-defined-out))
;assignment statements = set! (Set Bang)
;(set! x e)
; where x is already a variable in the environment
; DONT SET BANG TOP LEVEL BINDINGS
(define b 3)
(define f (lambda (x) (* 1 (+ x b))))
(define c (+ b 4)) ; 7
(set! b 5)
(define z (f 4))   ; 9
(define w c)       ; 7

;truth about cons

(define pr (cons 1 (cons #t "hi")))
(define lst (cons 1 (cons #t (cons "hi" null))))

;mcons cells
(define mpr (mcons 1(mcons #t "hi")))
(mcar mpr)
(mcdr mpr)
(set-mcdr! mpr 47)
(set-mcar! mpr 2)
(mcons mpr 47)
