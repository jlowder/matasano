#lang racket

(require "oracle.rkt")
(require "../2/xor.rkt")
(require "../1/base64.rkt")

(define input ":::::::::::::::::admin<true:::::::::::::")

(printf "using input: ~s\n" input)

(define ct (encryption_oracle input))

(printf "encrypted response (hex):\n0x~a\n" (bytes->hex ct))
(printf "result of decryption test on this result as-is: ~a\n" (if (decryption_oracle ct)
                                                                   "true"
                                                                   "false"))
(define bitmask (bytes-append (bytes 1)
                              (make-bytes 5 0)
                              (bytes 1)
                              (make-bytes 4 0)
                              (bytes 1)
                              (make-bytes 4 0)))
(printf "using this bitmask to muck with the cyphertext (at block #3): (hex) 0x~a\n" (bytes->hex bitmask))
(define mucked (bytes-append (subbytes ct 0 32)
                             (xor-bytes (subbytes ct 32 48) bitmask)
                             (subbytes ct 48)))

(printf "encrypted response after mucking with it (hex):\n0x~a\n" (bytes->hex mucked))
(printf "result of decryption test on the mucked response: ~a\n" (if (decryption_oracle mucked)
                                                                     "true **pass"
                                                                     "false **fail"))


