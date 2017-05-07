#lang racket

(require "../2/xor.rkt")
(require (planet vyzo/crypto:2:3))

(provide encrypt-aes-128-cbc
         decrypt-aes-128-cbc)

; note: no padding is added or removed.
;
; both of these functions work the same way: they recurse over the
; input byte string taking 16 at a time, either encrypting or
; decrypting and xor'ing where appropriate, and collecting the results
; into a list of byte strings. Then, the outer foldl iterates over the
; list and concatenates all the byte strings together to make one long
; byte string, which is returned. Each recursive step receives the iv
; to xor for this step, and passes the iv to use in the next step.

(define (encrypt-aes-128-cbc iv key bytes)
  (foldl (lambda (a b) (bytes-append b a)) #""
         (let ([encryptor (cipher-encrypt cipher:aes-128-ecb key iv #:padding #f)])
           (let xor-ecb ([iv iv]
                         [bytes bytes])
             (let ([ciphertext (cipher-update! encryptor (xor-bytes-ints (subbytes bytes 0 (min 16 (bytes-length bytes))) iv))])
               (cons ciphertext (if (<= (bytes-length bytes) 16)
                                    '()
                                    (xor-ecb ciphertext (subbytes bytes 16)))))))))

(define (decrypt-aes-128-cbc iv key bytes)
  (foldl (lambda (a b) (bytes-append b a)) #""
         (let ([decryptor (cipher-decrypt cipher:aes-128-ecb key iv #:padding #f)])
           (let xor-ecb ([iv iv]
                         [bytes bytes])
             (cons
              (xor-bytes-ints (cipher-update! decryptor (subbytes bytes 0 (min 16 (bytes-length bytes)))) iv)
              (if (<= (bytes-length bytes) 16)
                  '()
                  (xor-ecb (subbytes bytes 0 16) (subbytes bytes 16))))))))

