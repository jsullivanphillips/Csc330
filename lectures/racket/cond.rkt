; cond can be used to replace nested if statements
#lang racket

(provide (all-defined-out))

(define (sum3 xs)
  (cond [(null? xs) 0]
        [(number? (car xs)) (+ (car xs) (sum3 (cdr xs)))]
        [#t (+ (sum3 (car xs)) (sum3 (cdr xs)))]))

(define (sum4 xs)
  (cond [(null? xs) 0]
        [(number? (car xs)) (+ (car xs) (sum4 (cdr xs)))]
        [(list? (car xs)) (+ (sum4 (car xs)) (sum4 (cdr xs)))]
        [#t (sum4 (cdr xs))]))

;in racket, when evaluating an if expression,
;as long as the expression is not #f it results in true
(if 34 14 15) ;results in true, 14
(if null 14 15) ;results in true, 14
(if #f 14 15) ;only way this results false, 15