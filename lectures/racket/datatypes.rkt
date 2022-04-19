#lang racket

(provide (all-defined-out))

; helper functions where first element of list is a symbol
(define (Const i) (list 'Const i))
(define (Add e1 e2) (list 'Add e1 e2))

; helper functions that test what "kind of exp"
(define (Const? x) (eq? (car x) 'Const))
(define (Add? x) (eq? (car x) 'Add))

; helper functions that get the peieces for "one kind of exp"
(define (Const-int e) (car (cdr e)))
(define (Add-e1 e) (car (cdr e)))
(define (Add-e2 e) (car (cdr (cdr e))))

(define (eval-exp e)
  (cond [(Const? e) e]
        [(Add? e) (let ([v1 (Const-int (eval-exp (Add-e1 e)))]
                        [v2 (Const-int (eval-exp (Add-e2 e)))])
                    (Const (+v1 v2)))]
        [#t (error "eval-exp expected an exp")]))

;(foo e1 e2 e2)
;(foo? e)
;(foo-bar e) ;if e was made with (foo e1 e2 e3) then returns value in bar field
;(foo-baz e)
;(foo-quux e)
(struct foo (bar baz quux) #:transparent)

;each of the following builds a constructor, tester, and extractor
(struct const (int) #:transparent)
(struct negate (e) #:transparent)
(struct add (e1 e2) #:transparent)
(struct multiply (e1 e2) #:transparent)