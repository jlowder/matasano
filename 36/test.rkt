#lang racket

(require "../1/base64.rkt")
(require "../2/xor.rkt")
(require "../34/random.rkt")
(require "hmac256.rkt")
(require "sha256.rkt")
(require math)

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

(info "C & S           Agree on N=[NIST Prime], g=2, k=3, I (email), P (password)")
(define N 2410312426921032588552076022197566074856950548502459942654116941958108831682612228890093858261341614673227141477904012196503648957050582631942730706805009223062734745341073406696246014589361659774041027169249453200378729434170325843778659198143763193776859869524088940195577346119843545301547043747207749969763750084308926339295559968882457872412993810129130294592999947926365264059284647209730384947211681434464714438488520940127459844288859336526896320919633919)
(define g 2)
(define k 3)
(define I "ice@itsnotreal.com")
(define P #"vanilla")
(inspect N g k I P)

(info "S               1. Generate salt as random integer")
(define salt (number->bytes (random 64)))
(inspect salt)

(info "                2. Generate string xH=SHA256(salt|password)")
(define xH (sha256 (bytes-append salt P)))
(inspect xH)

(info "                3. Convert xH to integer x somehow (put 0x on hexdigest)")
(define x (bytes->number xH))
(inspect x)

(info "                4. Generate v=g**x % N")
(define v (modular-expt g x N))
(inspect v)

(info "C->S            Send I, A=g**a % N (a la Diffie Hellman)")
(define a (generate-random-number 192 (random 2147483647)))
(define A (modular-expt g a N))
(inspect I A)

(info "S->C            Send salt, B=kv + g**b % N")
(define b (generate-random-number 192 (random 2147483647)))
(define B (+ (* k v) (modular-expt g b N)))
(inspect salt B)

(info "S, C            Compute string uH = SHA256(A|B), u = integer of uH")
(define uH (sha256 (bytes-append (number->bytes A) (number->bytes B))))
(define u (bytes->number uH))
(inspect uH u)

(info "C               1. Generate string xH=SHA256(salt|password)")
(set! xH (sha256 (bytes-append salt P)))
(inspect xH)

(info "                2. Convert xH to integer x somehow (put 0x on hexdigest)")
(set! x (bytes->number xH))
(inspect x)

(info "                3. Generate S = (B - k * g**x)**(a + u * x) % N")
(define S (modular-expt (- B (* k (modular-expt g x N))) (+ a (* u x)) N))
(inspect S)

(info "                4. Generate K = SHA256(S)")
(define K (sha256 (number->bytes S)))
(inspect K)

(info "S               1. Generate S = (A * v**u) ** b % N")
(set! S (modular-expt (* A (modular-expt v u N)) b N))
(inspect S)

(info "                2. Generate K = SHA256(S)")
(define C-K K) ; save a copy for C
(set! K (sha256 (number->bytes S)))
(inspect K)

(info "C->S            Send HMAC-SHA256(K, salt)")
(define H-C-K (hmac256-bytes C-K salt))
(inspect H-C-K)

(info "S->C            Send \"OK\" if HMAC-SHA256(K, salt) validates")
(if (equal? H-C-K (hmac256-bytes K salt))
    (printf "OK\n")
    (printf "Not OK\n"))

