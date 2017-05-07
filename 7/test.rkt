#lang racket

(require "ecb.rkt")
(require "../1/base64.rkt")

(define key "YELLOW SUBMARINE")
(define data (base64file->bytes "gistfile1.txt"))

(printf "decrypting using aes-128-ecb with key: ~a\n" key)
(printf "\nOutput data:\n~a\n" (bytes->string/utf-8 (decrypt-aes-128-ecb (string->bytes/utf-8 key) data)))
