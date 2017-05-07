#lang racket

(require "../1/base64.rkt")
(require "../2/xor.rkt")
(require "../3/freq.rkt")
(require "key.rkt")

(define inputfile "gistfile1.txt")

(define data (base64file->bytes inputfile))

;(bytes->base64 data)
;(hamming (string->bytes/utf-8 "this is a test") (string->bytes/utf-8 "wokka wokka!!!")) ; should be 37

(define keysize (find-best-key-size (base64file->bytes inputfile)))

(define blocks (transpose-bytes (base64file->bytes inputfile) keysize))

(define found-key (list->bytes (for/list ([i blocks])
                               (first (find-best-xor (bytes->hex i))))))

(printf "found key:\n")
(bytes->string/utf-8 found-key)

(printf "\ndecoded text:\n~a\n" (bytes->string/utf-8 (xor-bytes-ints (base64file->bytes inputfile) found-key) #\.))
