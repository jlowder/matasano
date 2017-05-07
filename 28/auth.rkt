#lang racket

(require "sha1.rkt")
(provide authenticate validate)

(define generator (make-pseudo-random-generator))

(define (bytes->number bytes)
    (foldl (lambda (fb sb) (+ (* 256 sb) fb)) 0 (bytes->list bytes)))

(define (generate-random-key bytelen byteseed)
  (random-seed byteseed)
  (let ([ret (make-bytes bytelen 0)])
    (for ([i (in-range bytelen)])
      (bytes-set! ret i (random 256)))
    ret))

(define random-key (generate-random-key 8 (bytes->number (bytes (random 256 generator)
                                                                 (random 256 generator)
                                                                 (random 256 generator))))) ; seeded by current time


(define (authenticate message)
  (sha1 (bytes-append random-key message)))

(define (validate mac message)
  (equal? (authenticate message) mac))
