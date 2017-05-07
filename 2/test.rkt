#lang racket

(require "../1/base64.rkt")
(require "xor.rkt")

; from info.txt
(define hex1 "1c0111001f010100061a024b53535009181c")
(define hex2 "686974207468652062756c6c277320657965")
(define expected "746865206b696420646f6e277420706c6179")

(define bytes1 (hex->bytes hex1))
(define bytes2 (hex->bytes hex2))

(define result (bytes->hex (xor-bytes bytes1 bytes2)))

(printf "hex1:   ~a\n" hex1)
(printf "hex2:   ~a\n" hex2)

(printf "xor:    ~a\n" result)
(printf "expect: ~a\n" expected)

(if (equal? result expected)
    (printf "** pass\n")
    (printf "** fail\n"))
