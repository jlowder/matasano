#lang racket

; alternate version that does not use padding.

; Note: I added this file after submitting set1, once I realized that
; the library calls are internally handling padding. The test program,
; test.rkt, produces identical output when using these functions
; versus the ones in aes.rkt, since the library calls revert to
; non-padded versions if valid padding is not detected.

; These functions are derived from cbc.rkt in folder 10.
 
(require (planet vyzo/crypto:2:3))

(provide encrypt-aes-128-ecb
         decrypt-aes-128-ecb)
         
(define (encrypt-aes-128-ecb key bytes)
  (foldl (lambda (a b) (bytes-append b a)) #""
         (let ([encryptor (cipher-encrypt cipher:aes-128-ecb key (make-bytes 16 0) #:padding #f)])
           (let ecb ([bytes bytes])
             (let ([ciphertext (cipher-update! encryptor (subbytes bytes 0 (min 16 (bytes-length bytes))))])
               (cons ciphertext (if (<= (bytes-length bytes) 16)
                                    '()
                                    (ecb (subbytes bytes 16)))))))))

(define (decrypt-aes-128-ecb key bytes)
  (foldl (lambda (a b) (bytes-append b a)) #""
         (let ([decryptor (cipher-decrypt cipher:aes-128-ecb key (make-bytes 16 0)  #:padding #f)])
           (let ecb ([bytes bytes])
             (cons
              (cipher-update! decryptor (subbytes bytes 0 (min 16 (bytes-length bytes))))
              (if (<= (bytes-length bytes) 16)
                  '()
                  (ecb (subbytes bytes 16))))))))

