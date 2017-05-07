#lang racket
; functions for creating long random numbers as either bytestrings or
; bignums.
(provide generate-random-bytes generate-random-number bytes->number)

(define (bytes->number bytes)
    (foldl (lambda (fb sb) (+ (* 256 sb) fb)) 0 (bytes->list bytes)))

(define (generate-random-bytes bytelen byteseed)
  (random-seed byteseed)
  (let ([ret (make-bytes bytelen 0)])
    (for ([i (in-range bytelen)])
      (bytes-set! ret i (random 256)))
    ret))

(define (generate-random-number bytelen byteseed)
  (bytes->number (generate-random-bytes bytelen byteseed)))
