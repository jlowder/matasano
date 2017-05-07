#lang racket

(provide encrypt-aes-128-ctr
         decrypt-aes-128-ctr)

(require "../2/xor.rkt")
(require "../7/ecb.rkt")

(define (encrypt-aes-128-ctr nonce key data)
                                        ; iterate over chunks of 16 bytes until it is less than 16 bytes
                                        ; each chunk gets xor'd against a keystream, updated each block
  (foldl (lambda (a b) (bytes-append b a)) #""
         (let ctr ([bytes data]
                   [block-count 0])
           (let ([keystream (encrypt-aes-128-ecb key (bytes-append nonce (integer->integer-bytes block-count 8 #f #f)))])
             (cond 
              [(< (bytes-length bytes) 16) (cons (xor-bytes bytes (subbytes keystream 0 (bytes-length bytes))) '())]
              [else (cons (xor-bytes (subbytes bytes 0 16) keystream) (ctr (subbytes bytes 16) (add1 block-count)))])))))


(define (decrypt-aes-128-ctr nonce key data)
  (encrypt-aes-128-ctr nonce key data))
