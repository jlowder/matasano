#lang racket

(provide generate-reset-token)

(require "../21/mt19937.rkt")
(require "../1/base64.rkt")

(define (byte-fission value) ; convert a number into a list of bytes
  (reverse (let fission ([value value])
             (cond [(< value 256) (list value)]
                   [else (cons (bitwise-and 255 value)
                               (fission (arithmetic-shift value -8)))]))))

(define (generate-reset-token)
  ; seed mt19937 with current seconds
  ; generate 16 random bytes
  (initialize-generator (current-seconds))
  (let ([token (flatten (for/list ([i (in-range 4)])
                         (byte-fission (extract-number))))])
    (bytes->hex (list->bytes token))))

