; Empty list: null ( [] )
; Cons : cons (x::x's, x cons x's)
; head : car (hd)
; tail : cdr (pronounced 'kooder')(tl)
; check for empty: null?
; Can make list by (list e1 e2 ... en)
#lang racket

(provide (all-defined-out))
; Sum all numbers in a list
(define (sum xs)
    (if (null? xs)
    0
    (+ (car xs) (sum (cdr xs)))))

(define pp (list 1 2 3 4))
(sum pp)

; Append
(define (my-append xs ys)
    (if (null? xs)
        ys
        (cons (car xs) (my-append(cdr xs) ys))))


(define qq (list 2 "pp" 2 3 4 "a"))
(my-append pp qq)

; Map
(define (my-map f xs)
    (if (null? xs)
        null
        (cons (f (car xs)) 
              (my-map f (cdr xs)))))
        
(my-map (lambda (x) (+ x 1))
        (cons 3 (cons 4 (cons 5 null))))

(define (remove-head xs)
    (if (null? xs)
        null
        (cdr xs)))

(remove-head qq)
                
(define (count xs)
  (if (null? xs)
    0
    (if (number? (car xs))
        (+ 1 (count (cdr xs)))
        (count(cdr xs)))))

(count qq)
