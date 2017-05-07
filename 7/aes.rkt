#lang racket

(require "../1/base64.rkt")
(require (planet vyzo/crypto:2:3))

(provide encrypt-aes-128-ecb
         decrypt-aes-128-ecb)
         
(define (encrypt-aes-128-ecb key bytes)
  (encrypt cipher:aes-128-ecb key (make-bytes 16 0) bytes))

(define (decrypt-aes-128-ecb key bytes)
  (decrypt cipher:aes-128-ecb key (make-bytes 16 0) bytes))
