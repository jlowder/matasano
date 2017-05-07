#lang racket

(require "../1/base64.rkt")
(require "../2/xor.rkt")

(define input "Burning 'em, if you ain't quick and nimble\nI go crazy when I hear a cymbal")
(define key "ICE")
(define expected "0b3637272a2b2e63622c2e69692a23693a2a3c6324202d623d63343c2a26226324272765272a282b2f20430a652e2c652a3124333a653e2b2027630c692b20283165286326302e27282f")

(define result (xor-bytes-ints (string->bytes/utf-8 input) (string->bytes/utf-8 key)))

(printf "input:\n~a\n" input)
(printf "key:    ~a\n" key)
(printf "expect: ~a\n" expected)
(printf "result: ~a\n" (bytes->hex result))
(if (equal? (bytes->hex result) expected)
    (printf "** pass\n")
    (printf "** fail\n"))

(printf "\n")

; take the expected result, work backwards
(define inverse (xor-bytes-ints (hex->bytes expected) (string->bytes/utf-8 key)))
(printf "xor of expected string:\n~a\n" (bytes->string/utf-8 inverse))
(if (equal? (bytes->string/utf-8 inverse) input)
    (printf "** pass\n")
    (printf "** fail\n"))
