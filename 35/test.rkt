#lang racket

(require math)

(require "../28/sha1.rkt")
(require "../1/base64.rkt")
(require "../10/cbc.rkt")
(require "../9/pkcs7.rkt")
(require "../15/unpad.rkt")
(require "../34/random.rkt")

(define (number->bytes n)
  (hex->bytes (number->string n 16)))

; this defines a class to use for A and B
(define participant%
  (class object%
    (super-new)
    
    ; public fields supplied to constructor
    (init-field name)
    (init-field g) ; can be redefined later
    
    ; public fields
    (field (p 2410312426921032588552076022197566074856950548502459942654116941958108831682612228890093858261341614673227141477904012196503648957050582631942730706805009223062734745341073406696246014589361659774041027169249453200378729434170325843778659198143763193776859869524088940195577346119843545301547043747207749969763750084308926339295559968882457872412993810129130294592999947926365264059284647209730384947211681434464714438488520940127459844288859336526896320919633919))
    (field (op (open-output-bytes)))
    
    ; private attributes
    (define privkey (generate-random-number 192 (random 2147483647)))
    (field (pubkey (modular-expt g privkey p)))
    (define iv (generate-random-bytes 16 (random 2147483647)))
    (define sess 0)
    (define key #"")
    (define ip 0)

    (define/public (init-key other-pubkey)
      (set! sess (modular-expt other-pubkey privkey p))
      (set! key (subbytes (sha1 (number->bytes sess)) 0 16)))
    
    (define/public (init p g)
      (set! p p)
      (set! g g)
      (printf "p: ~a g: ~a~%" p g)
      (set! pubkey (modular-expt g privkey p)))

    (define/public (set-input-port input-port)
      (set! ip input-port))
    
    (define/public (encrypt-and-send m)
      (write-bytes (bytes-append iv (encrypt-aes-128-cbc iv key (pkcs7-pad-bytes m 16))) op))
    
    (define/public (reset)
      (begin
        (get-output-bytes op #t)
        "resetting participant"))

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
    (init-field init-func) ; function to muck with g
    (init-field session-func) ; function to generate session key
    (field (op1 (open-output-bytes))) ; B-facing
    (field (op2 (open-output-bytes))) ; A-facing
    (define p 0)
    (define g 0)
    (field (pubkey 0))
    (define ip 0)

    (define/public (init p g)
      (set! p p)
      (set! g (init-func p g))
      (send B init p g)
      (send A init p g))

    (define/public (init-key a-key)
      (send B init-key a-key)
      (send A init-key (get-field pubkey B)))

    
    (define/public (shovel-data)
      ; A->M            Send AES-CBC(SHA1(s)[0:16], iv=random(16), msg) + iv
      ; M->B            Relay that to B
      (let* ((ct (get-output-bytes (get-field op a)))
             (their-iv (subbytes ct 0 16))
             (key (subbytes (sha1 (number->bytes (session-func p))) 0 16))
             (pt (unpad (decrypt-aes-128-cbc their-iv key (subbytes ct 16)))))
        (printf "M: read from A: ~a~%" (bytes->hex ct))
        (printf "M: decrypting A's message to ~s~%" pt)
        (write-bytes ct op1) ; goes to B
        (send B decrypt-message))
      ; B->M            Send AES-CBC(SHA1(s)[0:16], iv=random(16), A's msg) + iv
      ; M->A            Relay that to A
      (let* ((ct (get-output-bytes (get-field op b)))
             (their-iv (subbytes ct 0 16))
             (key (subbytes (sha1 (number->bytes (session-func p))) 0 16))
             (pt (unpad (decrypt-aes-128-cbc their-iv key (subbytes ct 16)))))
        (printf "M: read from B: ~a~%" (bytes->hex ct))
        (printf "M: decrypting B's return message to ~s~%" pt)
        (write-bytes ct op2) ; goes back to A
        (send A decrypt-message)))
        ))

(define (setup-test a b)
  ; A->B            Send "p", "g"
  (send b init (get-field p a) (get-field g a))
  ; A->B            Send "A"
  (send b init-key (get-field pubkey a))
  ; B->A            Send "B"
  (send a init-key (get-field pubkey b))
  
  ; set their ports up so they send to each other
  (send a set-input-port (get-field op b))
  (send b set-input-port (get-field op a)))

(define (setup-mitm a m b)
  (send m init (get-field p a) (get-field g a))
  (send m init-key (get-field pubkey a))

  (send b set-input-port (get-field op1 m))
  (send a set-input-port (get-field op2 m)))

(define a (new participant% [name "A"] [g 2]))
(define b (new participant% [name "B"] [g 2]))

(printf "First: test without a MITM~%")
(setup-test a b)
(send a encrypt-and-send #"This is a test with no man in the middle.")
(send b decrypt-message)
(send a decrypt-message)

(printf "~%~%")
(printf "test with a MITM, g=1~%")
(send a reset)
(send b reset)
(define m (new man-in-the-middle%
               [A a]
               [B b]
               [init-func (lambda (p g) 1)]
               [session-func (lambda (p) 1)]))

(setup-mitm a m b)
(send a encrypt-and-send #"This is with a man in the middle using g=1")
(send m shovel-data)

(printf "~%~%")
(printf "test with a MITM, g=p~%")
(send a reset)
(send b reset)
(set! m (new man-in-the-middle%
               [A a]
               [B b]
               [init-func (lambda (p g) p)]
               [session-func (lambda (p) 0)]))

(setup-mitm a m b)
(send a encrypt-and-send #"This is with a man in the middle using g=p")
(send m shovel-data)

(printf "~%~%")
(printf "test with a MITM, g=(p-1)~%")
(send a reset)
(send b reset)
(set! m (new man-in-the-middle%
               [A a]
               [B b]
               [init-func (lambda (p g) (- p 1))]
               [session-func (lambda (p) (- p 1))]))

(setup-mitm a m b)
(send a encrypt-and-send #"This is with a man in the middle using g=(p-1)")
(send m shovel-data)

