#lang racket

(require math/number-theory)
(require "invmod.rkt")
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

(define plaintext "Luke, I am your father!")
(info "server's plaintext message:")
(inspect plaintext)

(define m (bytes->number (string->bytes/utf-8 plaintext)))
(inspect m)

(define (small-prime)
  (prev-prime (+ (generate-random-number 1 (random 2147483647)) (generate-random-number 1 (random 2147483647)))))

(define (big-prime)
  (prev-prime (generate-random-number 32 (random 2147483647))))

(info "Encrypt plaintext 3 times using different public keys")

(define rsa-participant%
  (class object%
    (super-new)
    (init-field e)
    (define p (big-prime))
    (define q (big-prime))
    (define n (* p q))
    (define et (* (- p 1) (- q 1)))
    (field (d (invmod e et)))

    ; Your public key is [e, n]. Your private key is [d, n].
    (field (public-key (list e n)))
    (field (private-key (list d n)))
    
    (define/public (decrypt-message c)
      (bytes->string/utf-8 (number->bytes (modulo (expt c d) n))))))

(define (gen-e3-particpant) ; keep trying until you get a "good" one, ie. coprime
  (let ((rsa (new rsa-participant% [e 3])))
    (if (equal? 0 (get-field d rsa))
        (gen-e3-particpant)
        rsa)))

(define (rsa-encrypt pubkey message)
  (modular-expt message (first pubkey) (second pubkey)))

(define rsa_0 (gen-e3-particpant))
(define rsa_1 (gen-e3-particpant))
(define rsa_2 (gen-e3-particpant))

(define n_0 (second (get-field public-key rsa_0)))
(inspect n_0)
(define n_1 (second (get-field public-key rsa_1)))
(inspect n_1)
(define n_2 (second (get-field public-key rsa_2)))
(inspect n_2)

(info "c_0, c_1, c_2 are the three respective residues mod n_0, n_1, n_2")

(define c_0 (rsa-encrypt (get-field public-key rsa_0) m))
(inspect c_0)
(define c_1 (rsa-encrypt (get-field public-key rsa_1) m))
(inspect c_1)
(define c_2 (rsa-encrypt (get-field public-key rsa_2) m))
(inspect c_2)

(info "m_s_n (for n in 0, 1, 2) are the product of the moduli")
(info "EXCEPT n_n --- ie, m_s_1 is n_0 * n_2")

(define m_s_0 (* n_1 n_2))
(inspect m_s_0)
(define m_s_1 (* n_0 n_2))
(inspect m_s_1)
(define m_s_2 (* n_0 n_1))
(inspect m_s_2)

(info "result =")
(info "  (c_0 * m_s_0 * invmod(m_s_0, n_0)) +")
(info "  (c_1 * m_s_1 * invmod(m_s_1, n_1)) +")
(info "  (c_2 * m_s_2 * invmod(m_s_2, n_2)) mod N_012")

(define result (modulo (+ 
                        (* c_0 m_s_0 (invmod m_s_0 n_0))
                        (* c_1 m_s_1 (invmod m_s_1 n_1))
                        (* c_2 m_s_2 (invmod m_s_2 n_2)))
                       (* n_0 n_1 n_2)))
(inspect result)

(define (cube-root x) ; using bigfloat to avoid precision errors
  (bigfloat->integer (bfround (bfexpt (bf x) (bf 1/3)))))

; Note: the instructions say to not do the modulo, but for whatever reason I still had to.

(bf-precision 2048) ; 2048 bits of floating-point precision
(inspect (cube-root result))

(define recovered-pt (bytes->string/utf-8 (number->bytes (cube-root result))))
(inspect recovered-pt)
