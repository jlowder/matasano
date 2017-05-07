#lang racket

(require math)

(require "../28/sha1.rkt")
(require "../1/base64.rkt")
(require "../10/cbc.rkt")
(require "../9/pkcs7.rkt")
(require "../15/unpad.rkt")

(require "random.rkt")

(define (number->bytes n)
  (hex->bytes (number->string n 16)))

; this defines a class to use for A and B
(define participant%
  (class object%
    (super-new)
    (init-field name)
    (field (p 2410312426921032588552076022197566074856950548502459942654116941958108831682612228890093858261341614673227141477904012196503648957050582631942730706805009223062734745341073406696246014589361659774041027169249453200378729434170325843778659198143763193776859869524088940195577346119843545301547043747207749969763750084308926339295559968882457872412993810129130294592999947926365264059284647209730384947211681434464714438488520940127459844288859336526896320919633919))
    (field (g 2))
    (field (op (open-output-bytes)))
    (define privkey (generate-random-number 192 (random 2147483647)))
    (field (pubkey (modular-expt g privkey p)))
    (define iv (generate-random-bytes 16 (random 2147483647)))
    (define sess 0)
    (define key #"")
    (define ip 0)

    (define/public (init-key other-pubkey)
      (set! sess (modular-expt other-pubkey privkey p))
      (set! key (subbytes (sha1 (number->bytes sess)) 0 16)))
    
    (define/public (init p g other-pubkey)
      (set! p p)
      (set! g g)
      (set! pubkey (modular-expt g privkey p))
      (send this init-key other-pubkey))

    (define/public (set-input-port input-port)
      (set! ip input-port))
    
    (define/public (encrypt-and-send m)
      (write-bytes (bytes-append iv (encrypt-aes-128-cbc iv key (pkcs7-pad-bytes m 16))) op))
    
    (define/public (decrypt-message)
      (let* ((m (get-output-bytes ip))
             (their-iv (subbytes m 0 16))
             (pt (unpad (decrypt-aes-128-cbc their-iv key (subbytes m 16)))))
        (printf "~a received: ~a~%" name (bytes->hex m))
        (printf "~a decrypted: ~s~%" name pt)
        (send this encrypt-and-send pt)))))

; this defines the man in the middle.
(define man-in-the-middle%
  (class object%
    (super-new)
    (init-field A)
    (init-field B)
    (field (op1 (open-output-bytes))) ; B-facing
    (field (op2 (open-output-bytes))) ; A-facing
    (define p 0)
    (define g 0)
    (field (pubkey 0))
    (define ip 0)

    (define/public (init p g other-pubkey)
      (set! p p)
      (set! g g)
      (set! pubkey p)
      ; M->B            Send "p", "g", "p"
      (send B init p g p)
      ; M->A            Send "p"
      (send A init-key p))
    
    (define/public (shovel-data)
      ; A->M            Send AES-CBC(SHA1(s)[0:16], iv=random(16), msg) + iv
      ; M->B            Relay that to B
      (let* ((ct (get-output-bytes (get-field op a)))
             (their-iv (subbytes ct 0 16))
             (key (subbytes (sha1 (number->bytes 0)) 0 16))
             (pt (unpad (decrypt-aes-128-cbc their-iv key (subbytes ct 16)))))
        (printf "M: read ~a from A~%" (bytes->hex ct))
        (printf "M: decrypting A's message to ~s~%" pt)
        (write-bytes ct op1) ; goes to B
        (send B decrypt-message))
      ; B->M            Send AES-CBC(SHA1(s)[0:16], iv=random(16), A's msg) + iv
      ; M->A            Relay that to A
      (let* ((ct (get-output-bytes (get-field op b)))
             (their-iv (subbytes ct 0 16))
             (key (subbytes (sha1 (number->bytes 0)) 0 16))
             (pt (unpad (decrypt-aes-128-cbc their-iv key (subbytes ct 16)))))
        (printf "M: read ~a from B~%" (bytes->hex ct))
        (printf "M: decrypting B's return message to ~s~%" pt)
        (write-bytes ct op2) ; goes back to A
        (send A decrypt-message)))
    ))
    
      
(define (setup-test a b)
  ; initialize b with p, g, and A from a
  (send b init (get-field p a) (get-field g a) (get-field pubkey a))

  ; now give b's B to a
  (send a init-key (get-field pubkey b))
  
  ; set their ports up so they send to each other
  (send a set-input-port (get-field op b))
  (send b set-input-port (get-field op a)))

(define (setup-mitm a m b)
  ; A->M            Send "p", "g", "A"
  (send m init (get-field p a) (get-field g a) (get-field pubkey a))
  
  ; make connections between A, B, and M
  (send b set-input-port (get-field op1 m))
  (send a set-input-port (get-field op2 m)))

(define a (new participant% [name "A"]))
(define b (new participant% [name "B"]))

(printf "First part: test without a MITM~%")
(setup-test a b)
(send a encrypt-and-send #"Logic clearly dictates that the needs of the many outweigh the needs of the few.")
(send b decrypt-message)
(send a decrypt-message)
(printf "~%~%")
(printf "Second part: test with a MITM~%")
; make new a and b objects
(set! a (new participant% [name "A"]))
(set! b (new participant% [name "B"]))
(define m (new man-in-the-middle% [A a] [B b]))
(setup-mitm a m b)
(send a encrypt-and-send #"Computers make excellent and efficient servants, but I have no wish to serve under them.")
(send m shovel-data)
