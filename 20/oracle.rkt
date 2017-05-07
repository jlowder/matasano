#lang racket

(provide ctr-encrypt)

(require "../18/ctr.rkt")
(require "../1/base64.rkt")

(define nonce (make-bytes 8 0))

(define key #"YELLOW SUBMARINE")

(define (ctr-encrypt data)
  (encrypt-aes-128-ctr nonce key data))
