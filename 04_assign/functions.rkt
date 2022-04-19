#lang racket


(provide (all-defined-out)) ;; so we can put tests in a second file

; part 1
(define nat-num-stream
  (letrec
      ([f (lambda (x)
            (cons x (lambda () (f (+ x 1)))))])
    (lambda () (f 0))))

;#1
(define (add-pointwise  xs ys)
  (cond [(and (not(list? xs)) (not(list? ys))) (error "illegal parameter")]
        [(and (null? xs) (null? ys)) null]
        [(null? xs) ys]
        [(null? ys) xs]
        [#t (cons (+ (car xs) (car ys)) (add-pointwise (cdr xs) (cdr ys)))]))


;#2
(define (add-pointwise-lists lst)
  (cond [(not(list? lst)) (error "illegal parameter")]
        [(null? lst) null]
        [(null? (cdr lst)) (car lst)]
        [#t (add-pointwise (car lst) (add-pointwise-lists (cdr lst)))]))
;#3
(define (add-pointwise-lists-2 lst)
  (if (null? lst)
  null
  (foldl add-pointwise null lst)))


;#4
(define (stream-for-n-steps s n)
  (letrec ([f (lambda (stream ans i)
                (let ([pr (stream)])
                  (if (= i n)
                      ans
                      (f (cdr pr) (append ans (list (car pr))) (+ i 1)))))])
  (f s null 0)))

;#5
(define fibo-stream
  (letrec ([f (lambda (x y) (cons x (lambda () (f (+ x y) x))))])
    (lambda () (f 0 1))))

;#6 takes a f and a stream, returns a stream
(define (filter-stream f s)
  (letrec ([aux (lambda (stream)
      (letrec ([pr (stream)])
      ;in
      
      (if (f (car pr))
        ;true branch
        (letrec ([foo (lambda () (cons (car pr) (filter-stream f (cdr pr))))])
        ;in
        (lambda () (foo)))
        ;end
        ;else branch
        (aux (cdr pr)))))])
     ;end
  ;in
  (aux s)))
  ;end

;#7
(define palyndromic-numbers
  (filter-stream (lambda (i)
       (letrec ([temp_str  (number->string i)]
                [char_list (string->list temp_str)]
                [reverse_list (reverse char_list)])
       (equal? char_list reverse_list)))
    nat-num-stream))
                        
                    
      

;#8 macro create-stream
(define-syntax create-stream                                                               ; macro name
  (syntax-rules (using starting at with increment)                                         ; other keywords
    [(create-stream name using f starting at i0 with increment delta)                      ; how to use macro
     (define name (letrec ([foo (lambda (x) (cons (f x) (lambda () (foo (+ x delta)))))])
                  (lambda () (foo i0))))                                                 ; form of expansion
     ]))

; part 2

;#1
(define vector-assoc #f)

;#2
(define cached-assoc #f)


