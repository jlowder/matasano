#lang racket

(require "pkcs7.rkt")
(require "../1/base64.rkt")

(define input #"YELLOW SUBMARINE")

(printf "testing ~s (hex ~s) at various paddings\n\n" input (bytes->hex input))
(for ([i (in-range 2 33)])
     (printf "~s padded to a multiple of ~a bytes:\n" (bytes->hex input) i)
     (printf "~s\n" (bytes->hex (pkcs7-pad-bytes input i)))
     ;(display (bytes->list (pkcs7-pad-bytes input i)))
     (printf "\n"))

     





