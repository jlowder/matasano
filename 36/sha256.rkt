#lang racket/base
; snarfed from http://rosettacode.org/wiki/SHA-256#Racket

(provide sha256)

;; define a quick SH256 FFI interface, similar to the Racket's default
;; SHA1 interface
(require ffi/unsafe ffi/unsafe/define openssl/libcrypto
         (only-in openssl/sha1 bytes->hex-string))
(define-ffi-definer defcrypto libcrypto)
(defcrypto SHA256_Init   (_fun _pointer -> _int))
(defcrypto SHA256_Update (_fun _pointer _pointer _long -> _int))
(defcrypto SHA256_Final  (_fun _pointer _pointer -> _int))
(define (sha256 bytes)
  (define ctx (malloc 128))
  (define result (make-bytes 32))
  (SHA256_Init ctx)
  (SHA256_Update ctx bytes (bytes-length bytes))
  (SHA256_Final result ctx)
  result)
 
