#lang racket

(require "../1/base64.rkt")
(require "../18/ctr.rkt")
(require "../7/aes.rkt")
(require "ctr.rkt")

(define inputfile "gistfile1.txt")

(define generator (make-pseudo-random-generator))

(define (bytes->number bytes)
    (foldl (lambda (fb sb) (+ (* 256 sb) fb)) 0 (bytes->list bytes)))

(define (generate-random-key bytelen byteseed)
  (random-seed byteseed)
  (let ([ret (make-bytes bytelen 0)])
    (for ([i (in-range bytelen)])
      (bytes-set! ret i (random 256)))
    ret))

(define random-key (generate-random-key 16 (bytes->number (bytes (random 256 generator)
                                                                 (random 256 generator)
                                                                 (random 256 generator))))) ; seeded by current time
(define key #"YELLOW SUBMARINE")
(define data (decrypt-aes-128-ecb key (base64file->bytes "gistfile1.txt")))

(define (decrypt-next-char ct known-portion) ; decrypt one more char, return the new known-portion
  (let* ([len (bytes-length known-portion)]
         [offset (- (bytes-length ct) (bytes-length known-portion) 1)])
    (for/or ([candidate (in-range 0 255)])
      (if (equal? ct (edit ct random-key offset (bytes-append (bytes candidate) known-portion)))
          (bytes-append (bytes candidate) known-portion)
          #f))))

(define (decrypt-ct ct known-portion) ; decide if ct is fully decrypted. If not, recurse with one more char decrypted.
  (cond
   [(equal? (bytes-length ct) (bytes-length known-portion)) known-portion]
   [else (decrypt-ct ct (decrypt-next-char ct known-portion))]))

; Data gets re-encrypted with a random key (also the nonce). To
; decrypt the final byte, edit the last byte with the candidate value
; (0..255), until the new ct equals the original one. Then, the final
; byte is known. Repeat for the previous byte, suffixing the known
; final byte afterwards, and so on.

(printf "Recovered plaintext: ~a\n" (bytes->string/utf-8 (decrypt-ct (encrypt-aes-128-ctr random-key random-key data) #"")))
