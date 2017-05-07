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

(define client%
  (class object%
    (super-new)
    
    ; public fields supplied to constructor
    (init-field N)
    (init-field g)
    (init-field k)
    (init-field I)
    (init-field P)
    
    ; private attributes
    (define salt #f)
    (define a (generate-random-number 192 (random 2147483647)))
    (field (A (modular-expt g a N)))
    ;(inspect N g k I P)

    (define/public (login-to-server srv fake-S)
      (let-values ([(NaCl B) (send srv login I A)])
        (inspect NaCl B)
        (set! salt NaCl)
        (let* ((uH (sha256 (bytes-append (number->bytes A) (number->bytes B))))
               (u (bytes->number uH))
               (xH (sha256 (bytes-append salt P)))
               (x (bytes->number xH))
               (v (modular-expt g x N))
               (S (if fake-S
                      fake-S
                      (modular-expt (- B (* k (modular-expt g x N))) (+ a (* u x)) N)))
               (K (sha256 (number->bytes S))))
          (printf "~s~%" (send srv verify (hmac256-bytes K salt))))))))

(define server%
  (class object%
    (super-new)
    
    ; public fields supplied to constructor
    (init-field N)
    (init-field g)
    (init-field k)
    (init-field I)
    (init-field P)
    
    ; private attributes
    (define salt (number->bytes (random 64)))
    (define b (generate-random-number 192 (random 2147483647)))
    (define xH (sha256 (bytes-append salt P)))
    (define x (bytes->number xH))
    (define v (modular-expt g x N))
    (define B (+ (* k v) (modular-expt g b N)))
    (define A #f)
    (define uH #f)
    (define u #f)
    (define K #f)
    (define S #f)

    (define/public (login in-I in-A)
      (inspect in-I in-A)
      (set! A in-A)
      (values salt B))
    
    (define/public (verify in-cert)
      (inspect in-cert)
      (set! uH (sha256 (bytes-append (number->bytes A) (number->bytes B))))
      (set! u (bytes->number uH))
      (set! S (modular-expt (* A (modular-expt v u N)) b N))
      (set! K (sha256 (number->bytes S)))
      (if (equal? in-cert (hmac256-bytes K salt))
          "OK"
          "FAIL"))))


(define clt (new client%
                 [N 2410312426921032588552076022197566074856950548502459942654116941958108831682612228890093858261341614673227141477904012196503648957050582631942730706805009223062734745341073406696246014589361659774041027169249453200378729434170325843778659198143763193776859869524088940195577346119843545301547043747207749969763750084308926339295559968882457872412993810129130294592999947926365264059284647209730384947211681434464714438488520940127459844288859336526896320919633919]
                 [g 2]
                 [k 3]
                 [I "ice@itsnotreal.com"]
                 [P #"vanilla"]))

(define srv (new server%
                 [N 2410312426921032588552076022197566074856950548502459942654116941958108831682612228890093858261341614673227141477904012196503648957050582631942730706805009223062734745341073406696246014589361659774041027169249453200378729434170325843778659198143763193776859869524088940195577346119843545301547043747207749969763750084308926339295559968882457872412993810129130294592999947926365264059284647209730384947211681434464714438488520940127459844288859336526896320919633919]
                 [g 2]
                 [k 3]
                 [I "ice@itsnotreal.com"]
                 [P #"vanilla"]))
(info "Following normal protocol")
(send clt login-to-server srv #f)

(info "Following normal protocol, but with invalid password")
(set-field! P clt #"VanWinkle")
(send clt login-to-server srv #f)

(info "Setting A to zero and forgetting password")
(set-field! A clt 0)
(set-field! P clt #"")
(send clt login-to-server srv 0)

(info "Setting A to N")
(set-field! A clt (get-field N clt))
(send clt login-to-server srv 0)

(info "Setting A to N*2")
(set-field! A clt (* 2 (get-field N clt)))
(send clt login-to-server srv 0)

(info "Setting A to N*5")
(set-field! A clt (* 5 (get-field N clt)))
(send clt login-to-server srv 0)

(info "Setting A to N*200")
(set-field! A clt (* 200 (get-field N clt)))
(send clt login-to-server srv 0)
