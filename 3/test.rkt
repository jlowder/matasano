#lang racket

(require "../1/base64.rkt")
(require "../2/xor.rkt")
(require "freq.rkt")

(define input "1b37373331363f78151b7f2b783431333d78397828372d363c78373e783a393b3736")

; iterate over all chars (0..255) 
; apply xor, generate new pattern
; determine score, see if it is new winner

(define best 100.0)
(define best-byte 0)
(define candidate 1.0)

(for [(i 255)]
     (set! candidate (score (bytes->string/utf-8 (xor-bytes-int (hex->bytes input) i) #\.)))
     (when (< candidate best) ; lower is better
       (set! best candidate)
       (set! best-byte i)))

(printf "Best candidate: ~a (ascii \"~a\") with score ~a\n" best-byte (integer->char best-byte) best)
(bytes->string/utf-8 (xor-bytes-int (hex->bytes input) best-byte) #\.)
