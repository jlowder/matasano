#lang racket/base

; vectors taken from http://en.wikipedia.org/wiki/MD4

(require "md4.rkt")
(require "../1/base64.rkt")

(define vectors '(#""
                  #"a"
                  #"abc"
                  #"message digest"
                  #"abcdefghijklmnopqrstuvwxyz"
                  #"ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789"
                  #"12345678901234567890123456789012345678901234567890123456789012345678901234567890"))

(define expected '("31d6cfe0d16ae931b73c59d7e0c089c0"
                   "bde52cb31de33e46245e05fbdbd6fb24"
                   "a448017aaf21d8525fc10ae87aa6729d"
                   "d9130a8164549fe818874806e1c7014b"
                   "d79e1c308aa5bbcdeea8ed63df412da9"
                   "043f8582f241db351ce627e153e7f0e4"
                   "e33b4ddc9c38f2199c3e7b164fcc0536"))

(for ([i vectors]
      [j expected])
     (printf "hashing ~s: ~s   ~a~%" i (bytes->hex (md4 i))  (cond [(equal? (bytes->hex (md4 i)) j) "**pass"]
                                                                   [else "**fail"])))
