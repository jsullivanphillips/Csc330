#lang racket


(provide (all-defined-out))





(define (my-append xs ys)
  (if (null? xs)
      ys
      (cons (car xs) (my-append (cdr xs) ys))))


(define p (list 1 2 3))

(define x (list 3 4 5))

(my-append x p)

(define (my-map f xs)
  (if (null? xs)
      null
      (cons (f (car xs)) (my-map f (cdr xs)))))

(my-map (lambda (x) (+ x 1)) (list 1 2 3))

(define (foo x)
  (cond [(or (= x 1)(= x 2)) 2]
        [#t 1]))
(foo 2)

(define xs (append null x))

(define lst (list (list 1 2 3) (list 1 2) (list 3 4)))
(define lala (cdr(cdr lst)))
;(cons (append (car lst) (car (cdr lst))) (cdr
(define nats
  (letrec ([f (lambda (x) (cons x (lambda () (f (+ x 1)))))])
    (lambda () (f 1))))
(define test01 (cons (car (nats)) (cdr (nats))))

(define (bar i)
       (letrec ([temp_str  (number->string i)]
                [char_list (string->list temp_str)]
                [reverse_list (reverse char_list)])
       (equal? char_list reverse_list)))

(define-syntax my-if             ; macro name
  (syntax-rules (then else)      ; other keywords
    [(my-if e1 then e2 else e3)  ; how to use macro
     (if e1 e2 e3)               ; form of expansion
     ]))