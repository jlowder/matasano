#lang racket

(require "../1/base64.rkt")
(require "../2/xor.rkt")
(require math)

(define (bytes->number bytes)
    (foldl (lambda (fb sb) (+ (* 256 sb) fb)) 0 (bytes->list bytes)))

(define (generate-random-key bytelen byteseed)
  (random-seed byteseed)
  (let ([ret (make-bytes bytelen 0)])
    (for ([i (in-range bytelen)])
      (bytes-set! ret i (random 256)))
    ret))

(define p 37)
(define g 5)
(define a (random p))
(define big-a (modular-expt g a p))
(define b (random p))
(define big-b (modular-expt g b p))
(printf "using values: a=~a, A=~a, b=~a, B=~a, g=~a, p=~a~%" a big-a b big-b g p)
(printf "keys: ~a ~a~%" (modular-expt big-a b p) (modular-expt big-b a p))
(printf "~a~%" (if (equal? (modular-expt big-a b p) (modular-expt big-b a p))
                   "**pass"
                   "**fail"))

(set! p 2410312426921032588552076022197566074856950548502459942654116941958108831682612228890093858261341614673227141477904012196503648957050582631942730706805009223062734745341073406696246014589361659774041027169249453200378729434170325843778659198143763193776859869524088940195577346119843545301547043747207749969763750084308926339295559968882457872412993810129130294592999947926365264059284647209730384947211681434464714438488520940127459844288859336526896320919633919)
(set! g 2)
(set! a (bytes->number (generate-random-key 192 (random 2147483647))))
(set! b (bytes->number (generate-random-key 192 (random 2147483647))))
(set! big-a (modular-expt g a p))
(set! big-b (modular-expt g b p))
(printf "using values: a=~a, A=~a, b=~a, B=~a, g=~a, p=~a~%" a big-a b big-b g p)
(printf "keys: ~a ~a~%" (modular-expt big-a b p) (modular-expt big-b a p))
(printf "~a~%" (if (equal? (modular-expt big-a b p) (modular-expt big-b a p))
                   "**pass"
                   "**fail"))
