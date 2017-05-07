#lang racket

(require "base64.rkt")

(define input    "49276d206b696c6c696e6720796f757220627261696e206c696b65206120706f69736f6e6f7573206d757368726f6f6d")
(define expected "SSdtIGtpbGxpbmcgeW91ciBicmFpbiBsaWtlIGEgcG9pc29ub3VzIG11c2hyb29t")

; convert the input to a byte array and then to base64
(define result (bytes->base64 (hex->bytes input)))

(printf "input:  ~a\n" input)
(printf "result: ~a\n" result)
(printf "expect: ~a\n" expected)

(if (equal? result expected)
    (printf "** pass\n")
    (printf "** fail\n"))

(printf "\n")

; convert the base64 to a byte array and then to hex, should get original input back
(define rev (bytes->hex (base64->bytes result)))
(printf "inverse: ~a\n" rev) 
(printf "expect:  ~a\n" input) 

(if (equal? rev input)
    (printf "** pass\n")
    (printf "** fail\n"))

