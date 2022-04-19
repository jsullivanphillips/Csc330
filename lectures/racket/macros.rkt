#lang racket

(provide (all-defined-out))

;;cosmetic macro
(define-syntax my-if  ; hey i am going to define a macro
  (syntax-rules (then else) ;definig keywords
    [(my-if e1 then e2 else e3) ;defining syntax of macro
     (if e1 e2 e3)])) ;what to replace macro with

(my-if #t then #f else #f) ;works like pattern matching

(define-syntax my-add
  (syntax-rules (ADD)
    [(my-add e1 ADD e2)
     (+ e1 e2)]))

(define-syntax comment-out
  (syntax-rules ()
    [(comment-out ignore instead) instead]))

;in a macro, you should use the paramater only once!
(define-syntax first-if-true
  (syntax-rules ()
    [(first-if-true e1 e2)
     (if e1 e1 e2)]))

(first-if-true (begin (println "hello") #f) (begin (println "again") 10))