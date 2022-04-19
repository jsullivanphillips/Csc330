#lang racket

(provide (all-defined-out))

;we want to represent a stream as a thunk
; but not any thunk, but a thunk that returns a pair
;
(define (repeat n th)
    (cond [(= n 0) #t]
          [#t (repeat (- n 1) (th))]))

