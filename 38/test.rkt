#lang racket

(require math)

(require "../1/base64.rkt")
(require "../34/random.rkt")
(require "../36/sha256.rkt")
(require "../36/hmac256.rkt")

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

(info "S               x = SHA256(salt|password)")
(define salt (number->bytes (random 64)))
(define P #"vanilla")
(define Sx (sha256 (bytes-append salt P)))
(inspect salt P Sx)

(info "                v = g**x % n")
(define g 2)
(define N 2410312426921032588552076022197566074856950548502459942654116941958108831682612228890093858261341614673227141477904012196503648957050582631942730706805009223062734745341073406696246014589361659774041027169249453200378729434170325843778659198143763193776859869524088940195577346119843545301547043747207749969763750084308926339295559968882457872412993810129130294592999947926365264059284647209730384947211681434464714438488520940127459844288859336526896320919633919)
(define v (modular-expt g (bytes->number Sx) N))
(inspect g v)

(info "C->S            I, A = g**a % n")
(define I "ice@itsnotreal.com")
(define a (generate-random-number 192 (random 2147483647)))
(define A (modular-expt g a N))
(inspect I A)

(info "S->C            salt, B = g**b % n, u = 128 bit random number")
(define b (generate-random-number 192 (random 2147483647)))
(define u (generate-random-number 16 (random 2147483647)))
(define B (modular-expt g b N))
(inspect salt B u)

(info "C               x = SHA256(salt|password)")
(define Cx (sha256 (bytes-append salt P)))
(inspect Cx)

(info "                S = B**(a + ux) % n")
(define Cs (modular-expt B (+ a (* u (bytes->number Cx))) N))
(inspect Cs)

(info "                K = SHA256(S)")
(define Ck (sha256 (number->bytes Cs)))
(inspect Ck)

(info "S               S = (A * v ** u)**b % n")
(define Ss (modular-expt (* A (modular-expt v u N)) b N))
(inspect Ss)

(info "                K = SHA256(S)")
(define Sk (sha256 (number->bytes Ss)))
(inspect Sk)

(info "C->S            Send HMAC-SHA256(K, salt)")
(define Client-HMAC-SHA256 (hmac256-bytes Ck salt))
(inspect Client-HMAC-SHA256)

(info "S->C            Send \"OK\" if HMAC-SHA256(K, salt) validates")
(if (equal? Client-HMAC-SHA256 (hmac256-bytes Sk salt))
    (~a "OK")
    (~a "FAIL"))

(printf "[Redoing with MITM attacker posing as server]~%")
(info "Seting salt to arbitrary value")
(set! salt (number->bytes 0))
(inspect salt)

(info "C->S            I, A = g**a % n")
(set! I "ice@itsnotreal.com")
(set! a (generate-random-number 192 (random 2147483647)))
(set! A (modular-expt g a N))
(inspect I a A)

(info "S->C            salt, B = g**b % n, u = 128 bit random number")
(set! b (generate-random-number 192 (random 2147483647)))
(set! u 1)
(set! B (modular-expt g b N))
(inspect salt B u)

(info "C               x = SHA256(salt|password)")
(set! Cx (sha256 (bytes-append salt P)))
(inspect Cx)

(info "                S = B**(a + ux) % n")
(set! Cs (modular-expt B (+ a (* u (bytes->number Cx))) N))
(inspect Cs)

(info "                K = SHA256(S)")
(set! Ck (sha256 (number->bytes Cs)))
(inspect Ck)

(info "C->S            Send HMAC-SHA256(K, salt)")
(set! Client-HMAC-SHA256 (hmac256-bytes Ck salt))
(inspect Client-HMAC-SHA256)
(printf "[performing dictionary search]~%")

(define words "/usr/share/dict/words")

(define word-list '())

(define (read-next-line-iter file)
  (let ((line (read-line file)))
    (unless (eof-object? line)
      (set! word-list (cons (string->bytes/utf-8 line) word-list))
      (read-next-line-iter file))))

(call-with-input-file words read-next-line-iter)

(if (for/or [(candidate word-list)]
      (set! P candidate)
      (set! Sx (sha256 (bytes-append salt P)))
      
      (set! v (modular-expt g (bytes->number Sx) N))
      
      (set! Ss (modular-expt (* A (modular-expt v u N)) b N))
      
      (set! Sk (sha256 (number->bytes Ss)))
      
      (equal? Client-HMAC-SHA256 (hmac256-bytes Sk salt)))
    (begin
      (printf "found the password!~%")
      (info P))
    (printf "unable to crack the password :-(~%"))

