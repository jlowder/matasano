#lang racket

(provide encryption-oracle check-decrypted-padding)

(require "../1/base64.rkt")
(require "../2/xor.rkt")
(require "../9/pkcs7.rkt")
(require "../10/cbc.rkt")
(require "../15/unpad.rkt")

(define inputs '("MDAwMDAwTm93IHRoYXQgdGhlIHBhcnR5IGlzIGp1bXBpbmc="
                 "MDAwMDAxV2l0aCB0aGUgYmFzcyBraWNrZWQgaW4gYW5kIHRoZSBWZWdhJ3MgYXJlIHB1bXBpbic="
                 "MDAwMDAyUXVpY2sgdG8gdGhlIHBvaW50LCB0byB0aGUgcG9pbnQsIG5vIGZha2luZw=="
                 "MDAwMDAzQ29va2luZyBNQydzIGxpa2UgYSBwb3VuZCBvZiBiYWNvbg=="
                 "MDAwMDA0QnVybmluZyAnZW0sIGlmIHlvdSBhaW4ndCBxdWljayBhbmQgbmltYmxl"
                 "MDAwMDA1SSBnbyBjcmF6eSB3aGVuIEkgaGVhciBhIGN5bWJhbA=="
                 "MDAwMDA2QW5kIGEgaGlnaCBoYXQgd2l0aCBhIHNvdXBlZCB1cCB0ZW1wbw=="
                 "MDAwMDA3SSdtIG9uIGEgcm9sbCwgaXQncyB0aW1lIHRvIGdvIHNvbG8="
                 "MDAwMDA4b2xsaW4nIGluIG15IGZpdmUgcG9pbnQgb2g="
                 "MDAwMDA5aXRoIG15IHJhZy10b3AgZG93biBzbyBteSBoYWlyIGNhbiBibG93"))

(define generator (make-pseudo-random-generator))

(define (bytes->number bytes)
    (foldl (lambda (fb sb) (+ (* 256 sb) fb)) 0 (bytes->list bytes)))

(define (generate-random-key bytelen byteseed)
  (random-seed byteseed)
  (let ([ret (make-bytes bytelen 0)])
    (for ([i (in-range bytelen)])
      (bytes-set! ret i (random 256)))
    ret))

(define random-key (generate-random-key 16 (bytes->number (bytes (random 256 generator)
                                                                 (random 256 generator)
                                                                 (random 256 generator))))) ; seeded by current time

(define (encryption-oracle)
  ; choose a random text, call encrypt-data
;  (encrypt-data (base64->bytes (list-ref inputs 0))))
(encrypt-data (base64->bytes (list-ref inputs (random (length inputs))))))

(define (encrypt-data data)
  (let ([random-iv (generate-random-key 16 (bytes->number (bytes (random 256 generator)
                                                                (random 256 generator)
                                                                (random 256 generator))))])
    (values (encrypt-aes-128-cbc random-iv random-key (pkcs7-pad-bytes data 16))
            random-iv)))
  
(define (check-decrypted-padding iv data)
  (let ([ret #t])
    (with-handlers ([exn:fail? (lambda (exn)
                                 (set! ret #f))])
      (unpad (decrypt-aes-128-cbc iv random-key data)))
    ret))


