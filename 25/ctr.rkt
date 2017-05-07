#lang racket

(provide edit)

(require "../18/ctr.rkt")

(define (edit cyphertext key offset newtext)
  (let* ([pt (decrypt-aes-128-ctr key key cyphertext)]
         [pre (subbytes pt 0 offset)])
    (encrypt-aes-128-ctr key key (bytes-append pre newtext))))


