#lang racket

;From wikipedia:
;
; // Create a length 624 array to store the state of the generator
; int[0..623] MT
; int index = 0
; 
; // Initialize the generator from a seed
; function initialize_generator(int seed) {
;     i := 0
;     MT[0] := seed
;     for i from 1 to 623 { // loop over each other element
;         MT[i] := last 32 bits of(1812433253 * (MT[i-1] xor (right shift by 30 bits(MT[i-1]))) + i) // 0x6c078965
;     }
; }
; 
; // Extract a tempered pseudorandom number based on the index-th value,
; // calling generate_numbers() every 624 numbers
; function extract_number() {
;     if index == 0 {
;         generate_numbers()
;     }
; 
;     int y := MT[index]
;     y := y xor (right shift by 11 bits(y))
;     y := y xor (left shift by 7 bits(y) and (2636928640)) // 0x9d2c5680
;     y := y xor (left shift by 15 bits(y) and (4022730752)) // 0xefc60000
;     y := y xor (right shift by 18 bits(y))
;
;     index := (index + 1) mod 624
;     return y
; }
; 
; // Generate an array of 624 untempered numbers
; function generate_numbers() {
;     for i from 0 to 623 {
;         int y := (MT[i] & 0x80000000)                       // bit 31 (32nd bit) of MT[i]
;                        + (MT[(i+1) mod 624] & 0x7fffffff)   // bits 0-30 (first 31 bits) of MT[...]
;         MT[i] := MT[(i + 397) mod 624] xor (right shift by 1 bit(y))
;         if (y mod 2) != 0 { // y is odd
;             MT[i] := MT[i] xor (2567483615) // 0x9908b0df
;         }
;     }
; }

(provide initialize-generator
         extract-number)

(define mt '())

(define index 0)

(define (initialize-generator seed)
  (set! index 0)
  (set! mt (cons seed 
                 (let step ([p seed]
                            [i 1])
                   (cond [(equal? i 624) '()]
                         [else (let ([x (bitwise-and 4294967295 (+ i (* 1812433253 (bitwise-xor p (arithmetic-shift p -30)))))])
                                 (cons x (step x (add1 i))))])))))


(define (extract-number)
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
          
             

        
  
