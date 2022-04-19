#lang racket

(provide (all-defined-out)) ;; so we can put tests in a second file

;; definition of structures for MUPL programs - Do NOT change
(struct var  (string) #:transparent)  ;; a variable, e.g., (var "foo")
(struct int  (num)    #:transparent)  ;; a constant number, e.g., (int 17)
(struct add  (e1 e2)  #:transparent)  ;; add two expressions
(struct ifgreater (e1 e2 e3 e4)    #:transparent) ;; if e1 > e2 then e3 else e4
(struct fun  (nameopt formal body) #:transparent) ;; a recursive(?) 1-argument function
(struct call (funexp actual)       #:transparent) ;; function call
(struct mlet (var e body) #:transparent) ;; a local binding (let var = e in body)
(struct apair (e1 e2)     #:transparent) ;; make a new pair
(struct fst  (e)    #:transparent) ;; get first part of a pair
(struct snd  (e)    #:transparent) ;; get second part of a pair
(struct aunit ()    #:transparent) ;; unit value -- good for ending a list
(struct isaunit (e) #:transparent) ;; evaluate to 1 if e is unit else 0

;; a closure is not in "source" programs; it is what functions evaluate to
(struct closure (env fun) #:transparent)

;; Problem A

;; CHANGE (put your solutions here)
(define (mupllist->racketlist lst)
  (define (aux rack_lst mup_lst)
    (if (aunit? lst)
        '()
    (letrec ([e1 (apair-e1 mup_lst)]
             [e2 (apair-e2 mup_lst)])
      (cond
        [(and (apair? e1) (apair? e2)) (aux (append rack_lst (list (mupllist->racketlist e1))) e2)]; If apair(apair(...), apair(...))
        [(and (not (aunit? e1)) (apair? e2)) (aux (append rack_lst (list e1)) e2)]; If apair(value, apair(...))
        [(and (apair? e1) (aunit? e2)) (append rack_lst (list (mupllist->racketlist e1)))]; If apair(apair(...), aunit)
        [(and (not (aunit? e1)) (aunit? e2)) (append rack_lst (list e1))]; If apair(value, aunit)                 
        ))))
  (aux '() lst))
     

(define (racketlist->mupllist lst)
  (define (aux mup_lst)
    (if (null? mup_lst)
      (aunit)
      (letrec ([head (car mup_lst)]
               [tail (cdr mup_lst)])
        (cond
          [(list? head) (apair (aux head) (aux tail))]
          [#t (apair head (aux tail))]
      ))))
  (aux lst))

;; Problem B

;; lookup a variable in an environment
;; Do NOT change this function
(define (envlookup env str)
  (cond [(null? env) (error "unbound variable during evaluation" str)]
        [(equal? (car (car env)) str) (cdr (car env))]
        [#t (envlookup (cdr env) str)]))

;; Do NOT change the two cases given to you.
;; DO add more cases for other kinds of MUPL expressions.
;; We will test eval-under-env by calling it directly even though
;; "in real life" it would be a helper function of eval-exp (see below).
(define (eval-under-env e env)
  (cond [(var? e)
         (envlookup env (var-string e))]
        [(add? e)
         (let ([v1 (eval-under-env (add-e1 e) env)]
               [v2 (eval-under-env (add-e2 e) env)])
           (if (and (int? v1)
                    (int? v2))
               (int (+ (int-num v1)
                       (int-num v2)))
               (error "MUPL addition applied to non-number")))]
        ;; "CHANGE" add more cases here
        ;; one for each type of expression
        [(call? e)
         (letrec ([closure (if (closure? (eval-under-env (call-funexp e) env)) (eval-under-env (call-funexp e) env) (error "Bad MUPL Call"))]
                  [cur-env  (closure-env closure)]
                  [funexp  (closure-fun closure)]
                  [actual (eval-under-env (call-actual e) cur-env)]
                  [formal (fun-formal funexp)]
                  [body (fun-body funexp)]
                  [binding (cons formal actual)]
                  [new-env (append env (list binding))])
           (eval-under-env body new-env))]
         
           
        [(fun? e)
           (closure '() e)]
        [(mlet? e)
         (letrec ([express-result (eval-under-env (mlet-e e) env)]
                  [cur-env (list (cons (mlet-var e) express-result))])
           (eval-under-env (mlet-body e) cur-env))]
        [(int? e)
         e]
        [(aunit? e)
         e]
        [(apair? e)
         (let ([v1 (eval-exp (apair-e1 e))]
               [v2 (eval-exp (apair-e2 e))])
               (apair v1 v2))]
        [(ifgreater? e)
         (letrec ([v1 (eval-exp (ifgreater-e1 e))]
                  [v2 (eval-exp (ifgreater-e2 e))]
                  [e3 (ifgreater-e3 e)]
                  [e4 (ifgreater-e4 e)]) 
           (if (and (int? v1) (int? v2))
               (if (> (int-num v1) (int-num v2))
                   (eval-exp e3)
                   (eval-exp e4))
               (error (format "bad MUPL if greater values: ~v ~v2" v1 v2))))]
        [(fst? e)
         (letrec ([v (fst-e e)]
                  [v2 (eval-under-env v env)])
           (if (apair? v2)
               (apair-e1 v2)
               (error (format "bad MUPL fst expression: ~v" e))))]
        [(snd? e)
         (letrec ([v (snd-e e)]
                  [v2 (eval-under-env v env)])
           (if (apair? v2)
               (apair-e2 v2)
               (error (format "bad MUPL fst expression: ~v" e))))]
        [(isaunit? e)
         (if (aunit? (eval-under-env (isaunit-e e) env))
             (int 1)
             (int 0))]
         
        [#t (error (format "bad MUPL expression: ~v" e))]))

;; Do NOT change
;; note how evaluating an expression start with an empty environment
(define (eval-exp e)
  (eval-under-env e null))

;; Problem C

(define (ifaunit e1 e2 e3) "CHANGE")

(define (mlet* lstlst e2) "CHANGE")

(define (ifeq e1 e2 e3 e4) "CHANGE")

;; Problem D

(define mupl-map "CHANGE")
;; this binding is a bit tricky. it must return a function.
;; the first two lines should be something like this:
;;
;;   (fun "mupl-map" "f"    ;; it is  function "mupl-map" that takes a function f
;;       (fun #f "lst"      ;; and it returns an anonymous function
;;          ...
;;
;; also remember that we can only call functions with one parameter, but
;; because they are curried instead of
;;    (call funexp1 funexp2 exp3)
;; we do
;;    (call (call funexp1 funexp2) exp3)
;; 

(define mupl-mapAddN
  (mlet "map" mupl-map
        "CHANGE (notice map is now in MUPL scope)"))
