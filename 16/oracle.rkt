#lang racket

(provide encryption_oracle decryption_oracle)

(require "../2/xor.rkt")
(require "../9/pkcs7.rkt")
(require "../10/cbc.rkt")
(require "../15/unpad.rkt")

(require json)

(define prefix (string->bytes/utf-8 "comment1=cooking%20MCs;userdata="))
(define suffix (string->bytes/utf-8 ";comment2=%20like%20a%20pound%20of%20bacon"))

(define (quote-specials str)
  (list->string
   (flatten
    (let ([strl (string->list str)])
      (let quote-char ([sl strl])
        (cond [(equal? (length sl) 0) '()]
              [(equal? (car sl) #\;) (cons '(#\% #\3 #\B) (quote-char (cdr sl)))]
              [(equal? (car sl) #\=) (cons '(#\% #\3 #\D) (quote-char (cdr sl)))]
              [else (cons (car sl) (quote-char (cdr sl)))]))))))

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

(define random-iv (generate-random-key 16 (bytes->number (bytes (random 256 generator)
                                                                (random 256 generator)
                                                                (random 256 generator))))) ; seeded by current time


(define (parse-cookie-string str) ; parse cookie string into json
  (let ([map (make-hash)])
    (for ([i ; split at ampersands, and then at semicolons
           (for/list ([i (regexp-split ";" str)])
             (regexp-split "=" i))])
      (hash-set! map (string->symbol (first i)) (second i)))
    (jsexpr->string map)))


(define (encryption_oracle input)
                                        ; use same random key each time
                                        ; combine prefix | input | suffix
                                        ; cbc encrypt with key and return
  (encrypt-aes-128-cbc random-iv random-key (pkcs7-pad-bytes (bytes-append prefix (string->bytes/utf-8 (quote-specials input)) suffix) 16)))

(define (decryption_oracle input)
  (if (regexp-match ";admin=true;" (bytes->string/utf-8 (unpad (decrypt-aes-128-cbc random-iv random-key input)) #\.))
      #t
      #f))
  
