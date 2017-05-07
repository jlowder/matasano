#lang racket

(provide receive-sample
         get-state
         distemper
         temper)

(define index 0)

(define mt (make-list 624 0))

(define (assign-element l i v) ; l[i] = v
  (append (take l i) (list v) (drop l (add1 i))))

(define (temper y)
  (set! y (bitwise-xor y (arithmetic-shift y -11)))
  (set! y (bitwise-xor y (bitwise-and (arithmetic-shift y 7) 2636928640)))
  (set! y (bitwise-xor y (bitwise-and (arithmetic-shift y 15) 4022730752)))
  (set! y (bitwise-xor y (arithmetic-shift y -18)))
  y)

; couple of utility functions to make 32-bit shifting easier

(define (lshift32 x n)
  (bitwise-and 4294967295 (arithmetic-shift x n)))

(define (rshift32 x n)
  (arithmetic-shift x (* -1 n)))

(define (distemper y)
  (set! y (bitwise-xor y (rshift32 y 18)))
  (set! y (bitwise-xor y (bitwise-and 4022730752 (lshift32 y 15))))
  (set! y (bitwise-xor y (bitwise-and 5760 (lshift32 y 7))))
  (set! y (bitwise-xor y (bitwise-and 802816 (lshift32 y 7))))
  (set! y (bitwise-xor y (bitwise-and 220200960 (lshift32 y 7))))
  (set! y (bitwise-xor y (bitwise-and 2415919104 (lshift32 y 7))))
  (set! y (bitwise-xor y (bitwise-and 4290772992 (rshift32 y 11))))
  (set! y (bitwise-xor y (bitwise-and 4192256 (rshift32 y 11))))
  (set! y (bitwise-xor y (bitwise-and 2047 (rshift32 y 11))))
  y)

(define (receive-sample prn)
  (let ([y prn])
    (set! mt (assign-element mt index (distemper y)))
    (set! index (modulo (add1 index) 624))))

(define (get-state)
  mt)
