#lang racket

(provide encryption_oracle)

(require "../1/base64.rkt")
(require "../2/xor.rkt")
(require "../7/ecb.rkt")
(require "../9/pkcs7.rkt")
(require "../10/cbc.rkt")

(define generator (make-pseudo-random-generator))

(define (bytes->number bytes)
    (foldl (lambda (fb sb) (+ (* 256 sb) fb)) 0 (bytes->list bytes)))

(define (generate-random-key bytelen byteseed)
; generate bytelen random bytes, using byteseed as the seed. This
; ensures that the same input generates the same random key each
; time. I assume this is the intended behavior since the instructions
; refer to this as an "oracle function".
;
; http://en.wikipedia.org/wiki/Random_oracle
;
    (random-seed byteseed)
    (let ([ret (make-bytes bytelen 0)])
      (for ([i (in-range bytelen)])
        (bytes-set! ret i (random 256)))
      ret))
  
(define (encryption_oracle input)
; generate random key
; pad beginning and end with random number of bytes
; run through ecb or cbc based on rand(2)
  (let ([key (generate-random-key 16 (remainder (bytes->number input) (sub1 (expt 2 31))))]
        [secs (ceiling (inexact->exact (current-inexact-milliseconds)))])
;    (random-seed (remainder secs (sub1 (expt 2 31)))) ; reseed with current time. The following stuff should not always
                                                      ; be a function of the input, like the key is.
        (let ([front-pad (make-bytes (+ 5 (random 6 generator)) 0)] ; instructions don't say what value to pad. using zeros.
              [back-pad (make-bytes (+ 5 (random 6 generator)) 0)]
              [iv (generate-random-key 16 (bytes->number (bytes (random 256 generator) (random 256 generator) (random 256 generator))))]
              [decision (random 2 generator)])
          (let ([padded (pkcs7-pad-bytes (bytes-append front-pad input back-pad) 16)])
            (if (equal? decision 0)
                (values (encrypt-aes-128-ecb key padded) "ECB")
                (values (encrypt-aes-128-cbc iv key padded) "CBC"))))))
            
          
    
  
