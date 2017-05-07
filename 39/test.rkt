#lang racket

(require math/number-theory)
(require "invmod.rkt") ; note that racket also has "modular-inverse" in the math module
(require math)

(require "../1/base64.rkt")
(require "../2/xor.rkt")
(require "../34/random.rkt")

(define (number->bytes n)
  (hex->bytes (number->string n 16)))

(define (generate-random-key bytelen byteseed)
  (random-seed byteseed)
  (let ([ret (make-bytes bytelen 0)])
    (for ([i (in-range bytelen)])
      (bytes-set! ret i (random 256)))
    ret))

(define (fmt x)
  (if (bytes? x)
      (string-append "0x" (string-upcase (bytes->hex x)))
      x))

(define-syntax-rule (inspect x ...)
  (begin (printf "~a=~a " 'x (fmt x)) ... (printf "\n")))

(define (info str)
  (printf "~a\n" (format "-------> ~a" str)))

(define (small-prime)
  (prev-prime (+ (generate-random-number 1 (random 2147483647)) (generate-random-number 1 (random 2147483647)))))

(define (big-prime)
  (prev-prime (generate-random-number 192 (random 2147483647))))

;(define seed (modulo (current-milliseconds) 2147483647))
(define seed 15288799) ; sometimes the randomly generated primes aren't coprime with "3", which breaks the modular inverse function. This is a good seed value that works for this test.
;(inspect seed)
(random-seed seed)

(info "- Generate 2 random primes. We'll use small numbers to start, so you")
(info "  can just pick them out of a prime table. Call them \"p\" and \"q\".")
(define p (small-prime))
(define q (small-prime))
(inspect p q)

(info "- Let n be p * q. Your RSA math is modulo n.")
(define n (* p q))
(inspect n)

(info "- Let et be (p-1)*(q-1) (the \"totient\"). You need this value only for")
(info "  keygen.")
(define et (* (- p 1) (- q 1)))
(inspect et)

(info "- Let e be 3.")
(define e 3)
(inspect e)

(info "- Compute d = invmod(e, et). invmod(17, 3120) is 2753.")
(define d (invmod e et))
(inspect d)

(info "Your public key is [e, n]. Your private key is [d, n].")
(define public-key (list e n))
(define private-key (list d n))
(inspect public-key private-key)

(info "To encrypt: c = m**e%n. To decrypt: m = c**d%n")
(info "Test this out with a number, like \"42\".")
(define m 42)
(define c (modulo (expt m e) n))
(define m1 (modulo (expt c d) n))
(inspect m c m1)

(info "Repeat with bignum primes (keep e=3).")
(set! p (big-prime))
(set! q (big-prime))
(inspect p q)
(set! n (* p q))
(inspect n)
(set! et (* (- p 1) (- q 1)))
(inspect et)
(set! d (invmod e et))
(inspect d)
(set! public-key (list e n))
(set! private-key (list d n))
(set! c (modular-expt m e n))
(set! m1 (modular-expt c d n))
(inspect m c m1)

(info "Finally, to encrypt a string, do something cheesy, like convert the")
(info "string to hex and put \"0x\" on the front of it to turn it into a")
(info "number. The math cares not how stupidly you feed it strings.")

(define plaintext "Good humor man kills 12")
(set! m (bytes->number (string->bytes/utf-8 plaintext)))
(set! c (modular-expt m e n))
(set! m1 (modular-expt c d n))
(define recovered-plaintext (bytes->string/utf-8 (number->bytes m1)))
(inspect plaintext m c m1 recovered-plaintext)

