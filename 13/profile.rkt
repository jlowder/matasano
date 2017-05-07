#lang racket

(provide encrypted-profile-for
         decrypt-profile)

(require "keyvalue.rkt")
(require "../7/aes.rkt")

(define generator (make-pseudo-random-generator))

(define (bytes->number bytes)
    (foldl (lambda (fb sb) (+ (* 256 sb) fb)) 0 (bytes->list bytes)))

(define (generate-random-key bytelen byteseed)
; generate bytelen random bytes, using byteseed as the seed.
    (random-seed byteseed)
    (let ([ret (make-bytes bytelen 0)])
      (for ([i (in-range bytelen)])
        (bytes-set! ret i (random 256)))
      ret))

(define random-key (generate-random-key 16 (bytes->number (bytes (random 256 generator)
                                                                 (random 256 generator)
                                                                 (random 256 generator)))))


(define (profile_for email) ; encode as a cookie string 
  (let ([clean (regexp-replace "[&=]" email "")])
    (string-append "email=" clean "&uid=10&role=user")))

(define (encrypted-profile-for email)
  (encrypt-aes-128-ecb random-key (string->bytes/utf-8 (profile_for email))))

(define (decrypt-profile bytes)
  (parse-cookie-string (bytes->string/utf-8 (decrypt-aes-128-ecb random-key bytes))))
