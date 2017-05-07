#lang racket

(require "oracle.rkt")
(require "../2/xor.rkt")
(require "../1/base64.rkt")
(require "../10/cbc.rkt")
(require "../15/unpad.rkt")

(define input "AAAAAAAAAAAAAAABBBBBBBBBBBBBBBCCCCCCCCCCCCCCC")

(printf "using input: ~s\n" input)

(define ct (encryption_oracle input))

(printf "encrypted response (hex):\n0x~a\n" (bytes->hex ct))
(printf "result of decryption test on this result as-is: ~a\n" (if (decryption_oracle ct)
                                                                   "true"
                                                                   "false"))
(let* ([c1 (subbytes ct 0 16)]
       [c2 (make-bytes 16 0)]
       [pt (decryption_oracle (bytes-append c1 c2 c1))]
       [p1 (subbytes pt 0 16)]
       [p3 (subbytes pt 32 48)]
       [key (xor-bytes p1 p3)]
       [recovered-pt (unpad (decrypt-aes-128-cbc key key ct))])
  (printf "derived key: ~s\n" (bytes->hex key))
  (printf "attempting to decrypt ct with derived key: ~s " (bytes->string/utf-8 recovered-pt))
  (if (equal? input (bytes->string/utf-8 recovered-pt))
      (display "**pass\n")
      (display "**fail\n")))



       
      
;(define bitmask (bytes-append (bytes 1)
;                              (make-bytes 5 0)
;                              (bytes 1)
;                              (make-bytes 4 0)
;                              (bytes 1)
;                              (make-bytes 4 0)))
;(printf "using this bitmask to muck with the cyphertext (at block #3): (hex) 0x~a\n" (bytes->hex bitmask))
;(define mucked (bytes-append (subbytes ct 0 32)
;                             (xor-bytes (subbytes ct 32 48) bitmask)
;                             (subbytes ct 48)))
;
;(printf "encrypted response after mucking with it (hex):\n0x~a\n" (bytes->hex mucked))
;(printf "result of decryption test on the mucked response: ~a\n" (if (decryption_oracle mucked)
;                                                                     "true **pass"
;                                                                     "false **fail"))
;
;
