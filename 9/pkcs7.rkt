#lang racket

(provide pkcs7-pad-bytes)

; see rfc5652.txt

(define (pkcs7-pad-bytes bytes length)
  (let ([padlen (- (* length (add1 (quotient (bytes-length bytes) length))) (bytes-length bytes))])
      (bytes-append bytes (make-bytes padlen padlen))))
      


