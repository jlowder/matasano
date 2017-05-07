#lang racket

(require "mtctr.rkt")
(require "../1/base64.rkt")
(require "pw-reset.rkt")
(require "../21/mt19937.rkt")

; encrypt and decrypt some stuff

(define inputs '(#"Take your stinking paws off me, you damned dirty ape!"
                 #"You know the saying, Human see, human do."
                 #"Doctor, would an ape make a human doll that TALKS?"
                 #"Some apes, it seems, are more equal than others."))

(define seed (modulo (current-seconds) 65535))
(define ct (for/list ([x inputs])
                     (bytes->hex (encrypt-mt11937-128-ctr seed x))))
(define pt (for/list ([x ct])
                     (decrypt-mt11937-128-ctr seed (hex->bytes x))))

(printf "using seed ~a\n" seed)

(for ([input inputs]
      [cyphertext ct]
      [plaintext pt])
     (printf "encrypting ~s\n0x~a\n" input cyphertext)
     (printf "decrypting 0x~a\n~s\n" cyphertext plaintext)
     (printf "**~a\n\n" (if (equal? plaintext input)
                            "pass"
                            "fail")))

; key recovery
(define known-pattern #"AAAAAAAAAAAAAA")
(define known-pattern-s (bytes->string/utf-8 known-pattern)) ; convert to text string so we can use regexp's on it

(define secret (bytes-append (list->bytes (build-list (+ 10 (random 40)) (lambda (x) (random 256)))) known-pattern))

(define ct-secret (encrypt-mt11937-128-ctr (modulo (current-seconds) 65535) secret))

(printf "cyphertext of partially known string:\n~s\n" (bytes->hex ct-secret))

(define (brute-decrypt seed)
  (when (equal? (modulo seed 1000) 0)
    (display seed)
    (display "\n"))
  (let ([xpt (bytes->string/utf-8 (decrypt-mt11937-128-ctr seed ct-secret) #\.)])
    (cond [(> seed 65535) #f]
          [(regexp-match known-pattern-s xpt) seed]
          [else (brute-decrypt (add1 seed))])))

(let ([key (brute-decrypt 0)])
  (if key
      (begin (printf "found key: ~a\n"key)
             (printf "decoded secret: ~s\n" (decrypt-mt11937-128-ctr key ct-secret))
             (printf "**~a\n" (if (equal? (decrypt-mt11937-128-ctr key ct-secret) secret)
                                  "pass"
                                  "fail")))
      (display "unable to find 16-bit key.\n")))

(define (bytes->number bytes)
    (foldl (lambda (fb sb) (+ (* 256 sb) fb)) 0 (bytes->list bytes)))

(define (hex->words hex) ; convert a hex string to a list of 32-bit words.
  (cond [(< (string-length hex) 8) '()]
        [else (cons (bytes->number (hex->bytes (substring hex 0 8))) (hex->words (substring hex 8)))]))
  
; generate reset tokens, test to see if they are mt19937 based

(define (mt19937-based? token)
  ; decode into words, see if originates from a recent unix timestamp
  (let ([w (hex->words token)])
    (for/or ([i (in-range (current-seconds) (- (current-seconds) 100) -1)])
            (initialize-generator i)
            (equal? (car w) (extract-number)))))

(define tokens (build-list 5 (lambda (x) (begin (sleep 1) (generate-reset-token)))))

(display "\nTesting token strings that \"are\" MT19937-based:\n")

(for ([x tokens])
     (printf "~s is ~a\n" x (if (mt19937-based? x)
                                                "based on MT19937\n**pass"
                                                "not based on MT19937\n**fail")))
; test negative cases
(display "\nTesting token strings that \"are not\" MT19937-based:\n")

(for ([x '("49474944a89b989"
           "8c90d433ffe3434"
           "435894893cde993"
           "343560fea0900ab"
           "c34095985984985")])
     (printf "~s is ~a\n" x (if (mt19937-based? x)
                                                "based on MT19937\n**fail"
                                                "not based on MT19937\n**pass")))
