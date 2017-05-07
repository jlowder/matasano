#lang racket

(require "../21/mt19937.rkt")
(require "../2/xor.rkt")

(provide encrypt-mt11937-128-ctr
         decrypt-mt11937-128-ctr)

(define (gen-8-bits)
  (bitwise-and (extract-number) 255))

(define (get-byte x) ; this is a mappable random byte generator, the parm is ignored.
  (gen-8-bits))

(define (encrypt-mt11937-128-ctr seed data)
  (initialize-generator (bitwise-and 65535 seed)) ; truncate seed to 16 bits
  (let ([keystream (list->bytes (build-list (bytes-length data) get-byte))])
    (xor-bytes data keystream)))

(define (decrypt-mt11937-128-ctr seed data)
  (encrypt-mt11937-128-ctr seed data))
