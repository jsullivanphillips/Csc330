#lang racket

(provide (all-defined-out))
; Top-level bindings
; - cannot shadow bindings (only one version of identifier per file) 
; - can forward reference (only use this in functions)
; (let ([x1 e1]
        ;       [x2 e2]
        ;       ...
        ;       [xn en]
        ;   body)

(define (max-of-list xs)
  (cond [(null? xs) (error "max-of-list given empty list")]
        [(null? (cdr xs)) (car xs)]
; Shoot to last element of list, and then return recursively
; On each hop, compare current element to the greatest value
; encountered while hoping back up the list. 
        [#t (let ([tlans (max-of-list (cdr xs))])
              (if (> tlans (car xs)) 
                  tlans
                  (car xs)))])) 

;three kinds of let bindings
; LET
; - can bind any number of variables
; - variables are in scope of environment from before the let-expression
(define (silly-double x)
  (let ([x (+ x 3)] ;x on the right side refers to param
        [y (+ x 2)]);x on the right side refers to param
     (+ x y -5)))   ;x refers to the let binding
; LET*
; - same as let, expressions are evaluated from the previous bindings
;   in the environment
; - similar to ML  let
(define (silly-double1 x)
  (let* ([x (+ x 3)] ;x on the right side refers to param
        [y (+ x 2)]);x on the right side refers to param
     (+ x y -8)))   ;x refers to the let binding
; LETREC
; - let recursive. can use bindings after line
; - needed for mutually recursive functions
(define (silly-triple x)
  (letrec ([y (+ x 2)]
           [f (lambda(z) (+ z y w x))] ;can use w even though it is defined after
           [w (+ x 7)])
    (f -9)))

;good use of letrec
(define (silly-mod2 x)
  (letrec
      ([even? (lambda(x) (if (zero? x) #t (odd? (- x 1))))]
       [odd?  (lambda(x) (if (zero? x) #f (even? (- x 1))))])
     (if (even? x) 0 1)))

