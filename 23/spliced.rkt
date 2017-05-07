#lang racket
;
; This version is almost identical to the one in problem 21, but there
; is no "initialize-generator" function. Instead the state can be
; explicitly set via "store-state".
;
(provide predict-number store-state)

(define mt '())

(define index 0)

(define (store-state i table)
  (set! index i)
  (set! mt table))

(define (predict-number)
  (when (equal? 0 index)
    (generate-numbers))
  (let ([y (list-ref mt index)])
    (set! y (bitwise-xor y (arithmetic-shift y -11)))
    (set! y (bitwise-xor y (bitwise-and (arithmetic-shift y 7) 2636928640)))
    (set! y (bitwise-xor y (bitwise-and (arithmetic-shift y 15) 4022730752)))
    (set! y (bitwise-xor y (arithmetic-shift y -18)))
    (set! index (modulo (add1 index) 624))
    y))

(define (assign-element l i v) ; l[i] = v
  (append (take l i) (list v) (drop l (add1 i))))

(define (generate-numbers)
  (for ([i (in-range 0 624)])
    (let ([y (+ (bitwise-and (list-ref mt i) 2147483648)
                (bitwise-and (list-ref mt (modulo (add1 i) 624)) 2147483647))])
      (set! mt (assign-element mt i (bitwise-xor (list-ref mt (modulo (+ i 397) 624)) (arithmetic-shift y -1))))
      (when (not (equal? 0 (modulo y 2)))
        (set! mt (assign-element mt i (bitwise-xor (list-ref mt i) 2567483615)))))))
          
             

        
  
