#lang racket

(require rackunit "hw5.rkt")


(eval-exp (mlet "f1"
                               (fun #f "a" (mlet "x"
                                                 (var "a")
                                                 (fun #f "z" (add (var "x") (int 1)))))
                               (mlet "f3"
                                     (fun #f "f"
                                          (mlet "x"
                                                (int 1729)
                                                (call (var "f") (aunit))))
                                     (call (var "f3") (call (var "f1") (int 1))))))