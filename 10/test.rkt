#lang racket

(require "cbc.rkt")
(require "../1/base64.rkt")

(define data (base64file->bytes "gistfile1.txt"))
(define key #"YELLOW SUBMARINE")
(define iv (make-bytes 16 (char->integer #\0)))

(printf "decrypting using CBC mode, with key:\n")
key
(printf "and IV:\n")
iv
(decrypt-aes-128-cbc iv key data)
