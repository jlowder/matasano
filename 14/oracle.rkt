#lang racket

(provide encryption_oracle)

(require "../1/base64.rkt")
(require "../2/xor.rkt")
(require "../7/aes.rkt")

(define generator (make-pseudo-random-generator))
(define suffix (base64->bytes "Um9sbGluJyBpbiBteSA1LjAKV2l0aCBteSByYWctdG9wIGRvd24gc28gbXkgaGFpciBjYW4gYmxvdwpUaGUgZ2lybGllcyBvbiBzdGFuZGJ5IHdhdmluZyBqdXN0IHRvIHNheSBoaQpEaWQgeW91IHN0b3A/IE5vLCBJIGp1c3QgZHJvdmUgYnkK"))

(define prefix (list->bytes (for/list ([i (in-range 0 (random 128 generator))]) ; random number of random bytes
                 (random 256))))

(define (bytes->number bytes)
    (foldl (lambda (fb sb) (+ (* 256 sb) fb)) 0 (bytes->list bytes)))

(define (generate-random-key bytelen byteseed)
; generate bytelen random bytes, using byteseed as the seed.
    (random-seed byteseed)
    (let ([ret (make-bytes bytelen 0)])
      (for ([i (in-range bytelen)])
        (bytes-set! ret i (random 256)))
      ret))

(define random-key (generate-random-key 16 (bytes->number (bytes (random 256 generator)
                                                                 (random 256 generator)
                                                                 (random 256 generator))))) ; seeded by current time
  
(define (encryption_oracle input)
; use same random key each time
; combine input and suffix
; ecb encrypt with key and return
  (encrypt-aes-128-ecb random-key (bytes-append prefix input suffix)))
            
          
    
  
